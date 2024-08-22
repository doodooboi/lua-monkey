---@diagnostic disable: undefined-field, undefined-global
local object = require("components.object")
local parser = require("components.parser")
local lexer = require("components.lexer")

require("utility.utility")
require("components.evaluator")

---@param input string
---@return BaseObject
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
			{ "true",                                         true },
			{ "false",                                        false },
			{ "1 < 2",                                        true },
			{ "1 > 2",                                        false },
			{ "1 < 1",                                        false },
			{ "1 > 1",                                        false },
			{ "1 == 1",                                       true },
			{ "1 != 1",                                       false },
			{ "1 == 2",                                       false },
			{ "1 != 2",                                       true },
			{ "true == true",                                 true },
			{ "false == false",                               true },
			{ "true == false",                                false },
			{ "true != false",                                true },
			{ "false != true",                                true },
			{ "(1 < 2) == true",                              true },
			{ "(1 < 2) == false",                             false },
			{ "(1 > 2) == true",                              false },
			{ "(1 > 2) == false",                             true },
			{ '"Hello, world!" == "Hello" + ", " + "world!"', true }
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
			{ "foobar",            "identifier not found: foobar" },
			{ '"Hello" - "World"', "unknown operator: STRING - STRING" }
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

	it("can do functions", function()
		local input = { "fn(x) { x + 2; };", "(x + 2)" }

		local evaluated = testEval(input[1])
		expect(object.types.FUNCTION_OBJ, evaluated)

		---@cast evaluated Function
		assert.are_equal(1, #evaluated.Parameters, string.format(
			"Expected 1 function parameter, got %s!",
			#evaluated.Parameters
		))

		assert.are_equal("x", tostring(evaluated.Parameters[1]), string.format(
			"Expected parameter to be %s, got %s",
			"x",
			tostring(evaluated.Parameters[1])
		))

		assert.are_equal("(x + 2)", tostring(evaluated.Body), string.format(
			"Expected body to be (x + 2), got %s",
			tostring(evaluated.Body)
		))
	end)

	it("can do actual functions", function()
		local tests = {
			{ "let identity = fn(x) { x; }; identity(5);",             5 },
			{ "let identity = fn(x) { return x; }; identity(5);",      5 },
			{ "let double = fn(x) { x * 2; }; double(5);",             10 },
			{ "let add = fn(x, y) { x + y; }; add(5, 5);",             10 },
			{ "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20 },
			{ "fn(x) { x; }(5)",                                       5 }
		}

		for _, test in ipairs(tests) do
			testObject(testEval(test[1]), object.types.INTEGER_OBJ, test[2])
		end
	end)

	it("can do strings", function()
		local input = '"Hello, world!"'

		local evaluated = testEval(input)
		expect(object.types.STRING_OBJ, evaluated)

		---@cast evaluated String
		assert.are_equal("Hello, world!", evaluated.Value, string.format(
			"String expected %s, got %s",
			input,
			evaluated.Value
		))
	end)

	it("can concatenate strings", function()
		local input = '"Hello" + ", " + "world!"'

		local evaluated = testEval(input)
		expect(object.types.STRING_OBJ, evaluated)

		---@cast evaluated String
		assert.are_equal("Hello, world!", evaluated.Value, string.format(
			"String expected %s, got %s",
			"\"Hello, world!\"",
			evaluated.Value
		))
	end)

	-- Once hash maps get implemented, I want to add them builtins into those, rather than global
	it("can call builtin functions", function()
		local tests = {
			{ 'len("")',            0 },
			{ 'len("four")',        4 },
			{ 'len("hello world")', 11 },
			{ 'len(1)',             "expected STRING, got INTEGER" },
			{ 'len("one", "two")',  "expected 1 argument, got 2" }
		}

		for _, test in ipairs(tests) do
			local evaluated = testEval(test[1])

			if evaluated:Type() == object.types.INTEGER_OBJ then
				testObject(evaluated, object.types.INTEGER_OBJ, test[2])
			else
				expect(object.types.ERROR_OBJ, evaluated)
				---@cast evaluated Error

				assert.are_equal(test[2], evaluated.Message, string.format(
					"Expected error '%s', got '%s'",
					test[2],
					evaluated.Message
				))
			end
		end
	end)

	it("can evaluate array indexing", function()
		local input = "[1, 2 * 2, 3 + 3]"

		local evaluated = testEval(input)
		expect(object.types.ARRAY_OBJ, evaluated)

		---@cast evaluated Array

		assert.are_equal(3, #evaluated.Elements, string.format(
			"Expected %s elements, got %s",
			3,
			#evaluated.Elements
		))

		testObject(evaluated.Elements[1], object.types.INTEGER_OBJ, 1)
		testObject(evaluated.Elements[2], object.types.INTEGER_OBJ, 4)
		testObject(evaluated.Elements[3], object.types.INTEGER_OBJ, 6)
	end)

	it("can really index arrays", function()
		local tests = {
			{ "[1, 2, 3][0]",                                                   1 },
			{ "[1, 2, 3][1]",                                                   2 },
			{ "[1, 2, 3][2]",                                                   3 },
			{ "let i = 0; [1][i];",                                             1 },
			{ "[1, 2, 3][1 + 1];",                                              3 },
			{ "let myArray = [1, 2, 3]; myArray[2];",                           3 },
			{ "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];", 6 },
			{ "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",        2 },
			{ "[1, 2, 3][3]",                                                   nil },
			{ "[1, 2, 3][-1]",                                                  nil },
		}

		for _, test in ipairs(tests) do
			local evaluated = testEval(test[1])

			if test[2] then
				testObject(evaluated, object.types.INTEGER_OBJ, test[2])
			else
				testObject(evaluated, object.types.NULL_OBJ)
			end
		end
	end)
end)
