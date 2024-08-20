---@diagnostic disable: undefined-field, undefined-global
local object = require("components.object")
local parser = require("components.parser")
local lexer = require("components.lexer")

require("utility.utility")
require("components.evaluator")

---@param input string
---@return any
local function testEval(input)
	local lexed = lexer.new(input)
	local parsed = parser.new(lexed)

	local program = parsed:ParseProgram()

	return eval(program, object.Environment.new())
end

---@param what string
---@param expr BaseObject
---@return nil
local function expect(what, expr)
	assert.are_equal(what, typeof(expr),
		string.format("Expected expression to be an %s, got %s", what, typeof(expr))
	)
end

---@param obj BaseObject
---@param expectedType string
---@param expectedValue any
---@return boolean?
local function testObject(obj, expectedType, expectedValue)
	expect(expectedType, obj)

	assert.are_equal(expectedValue, obj.Value, string.format(
		"Expected object.Value to be %s, got %s",
		expected,
		obj.Value
	))

	return true
end

describe("the evaluator", function()
	it("can do integer expressions", function()
		local tests = {
			{ "5",                               5 },
			{ "10",                              10 },
			{ "-5",                              -5 },
			{ "-10",                             -10 },
			{ "5 + 5 + 5 + 5 - 10",              10 },
			{ "2 * 2 * 2 * 2 * 2",               32 },
			{ "-50 + 100 + -50",                 0 },
			{ "5 * 2 + 10",                      20 },
			{ "5 + 2 * 10",                      25 },
			{ "20 + 2 * -10",                    0 },
			{ "50 / 2 * 2 + 10",                 60 },
			{ "2 * (5 + 10)",                    30 },
			{ "3 * 3 * 3 + 10",                  37 },
			{ "3 * (3 * 3) + 10",                37 },
			{ "(5 + 10 * 2 + 15 / 3) * 2 + -10", 50 },
		}

		for _, test in ipairs(tests) do
			local result = testEval(test[1])

			testObject(result, object.types.INTEGER_OBJ, test[2])
		end
	end)

	it("can do boolean expressions", function()
		local tests = {
			{ "true",             true },
			{ "false",            false },
			{ "1 < 2",            true },
			{ "1 > 2",            false },
			{ "1 < 1",            false },
			{ "1 > 1",            false },
			{ "1 == 1",           true },
			{ "1 != 1",           false },
			{ "1 == 2",           false },
			{ "1 != 2",           true },
			{ "true == true",     true },
			{ "false == false",   true },
			{ "true == false",    false },
			{ "true != false",    true },
			{ "false != true",    true },
			{ "(1 < 2) == true",  true },
			{ "(1 < 2) == false", false },
			{ "(1 > 2) == true",  false },
			{ "(1 > 2) == false", true },
		}

		for _, test in ipairs(tests) do
			local result = testEval(test[1])

			testObject(result, object.types.BOOLEAN_OBJ, test[2])
		end
	end)

	it("can do bang expressions", function()
		local tests = {
			{ "!true",   false },
			{ "!false",  true },
			{ "!5",      false },
			{ "!!true",  true },
			{ "!!false", false },
			{ "!!5",     true },
		}

		for _, test in ipairs(tests) do
			local result = testEval(test[1])
			testObject(result, object.types.BOOLEAN_OBJ, test[2])
		end
	end)

	it("can do if expressions", function()
		local tests = {
			{ "if (true) { 10 }",              10 },
			{ "if (false) { 10 }",             nil },
			{ "if (1) { 10 }",                 10 },
			{ "if (1 < 2) { 10 }",             10 },
			{ "if (1 > 2) { 10 }",             nil },
			{ "if (1 > 2) { 10 } else { 20 }", 20 },
			{ "if (1 < 2) { 10 } else { 20 }", 10 },
		}

		for _, test in ipairs(tests) do
			local result = testEval(test[1])

			if test[2] then
				testObject(result, object.types.INTEGER_OBJ, test[2])
			else
				testObject(result, object.types.NULL_OBJ)
			end
		end
	end)

	it("can do returns", function()
		local tests = {
			{ "return 10;",          10 },
			{ "return 10; 9;",       10 },
			{ "return 2 * 5; 9;",    10 },
			{ "9; return 2 * 5; 9;", 10 },
			{ [[
                if (10 > 1) {
                  if (10 > 1) {
                    return 10;
                  }

                  return 1;
                }
            ]], 10 }
		}

		for _, test in ipairs(tests) do
			local evaluated = testEval(test[1])

			testObject(evaluated, object.types.INTEGER_OBJ, test[2])
		end
	end)

	it("can produce errors", function()
		local tests = {
			{ "5 + true;",                     "type mismatch: INTEGER + BOOLEAN" },
			{ "5 + true; 5;",                  "type mismatch: INTEGER + BOOLEAN" },
			{ "-true",                         "unknown operator: -BOOLEAN" },
			{ "true + false;",                 "unknown operator: BOOLEAN + BOOLEAN" },
			{ "5; true + false; 5",            "unknown operator: BOOLEAN + BOOLEAN" },
			{ "if (10 > 1) { true + false; }", "unknown operator: BOOLEAN + BOOLEAN" },
			{ [[
                if (10 > 1) {
                  if (10 > 1) {
                    return true + false;
                  }
                  return 1;
                }
            ]], "unknown operator: BOOLEAN + BOOLEAN"
			},
			{ "foobar", "identifier not found: foobar" }
		}

		for _, test in ipairs(tests) do
			local evaluated = testEval(test[1])

			expect(object.types.ERROR_OBJ, evaluated)
			---@cast evaluated Error

			assert.are_equal(test[2], evaluated.Message, string.format(
				"Expected error message: '%s', got '%s'",
				test[2],
				evaluated.Message
			))
		end
	end)

	it("can do let statements", function()
		local tests = {
			{ "let a = 5; a;",                               5 },
			{ "let a = 5 * 5; a;",                           25 },
			{ "let a = 5; let b = a; b;",                    5 },
			{ "let a = 5; let b = a; let c = a + b + 5; c;", 15 }
		}

		for _, test in ipairs(tests) do
			testObject(testEval(test[1]), object.types.INTEGER_OBJ, test[2])
		end
	end)
end)
