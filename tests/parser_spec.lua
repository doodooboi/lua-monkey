---@diagnostic disable: undefined-global, undefined-field
local tokens = require("components.tokens")
local parser = require("components.parser")
local lexer = require("components.lexer")
local ast = require('components.ast')

require("utility.utility")

local function expect(what, expr)
	assert.are_equal(what, typeof(expr),
		string.format("Expected expression to be an %s, got %s", what, typeof(expr))
	)
end

---@param stmt LetStatement
---@param name string
local function testLetStatement(stmt, name)
	assert.are_equal("let", stmt:TokenLiteral())
	expect("LetStatement", stmt)

	assert.are_equal(name, stmt.Name.Value)
	assert.are_equal(name, stmt.Name:TokenLiteral())
end

---@param expr Expression
---@param value number
local function testIntegerLiteral(expr, value)
	assert.are_equal("IntegerLiteral", typeof(expr),
		string.format("Expected statement.Expression to be an IntegerLiteral, got %s", typeof(expr))
	)

	---@cast expr IntegerLiteral
	local absolute = tostring(value)

	assert.are_equal(value, expr.Value,
		string.format("Expected statement.Expression.Value to be %s, got %s", value, expr.Value)
	)

	assert.are_equal(absolute, expr:TokenLiteral(),
		string.format("Expected statement.Expression::TokenLiteral to be '%s', got %s", absolute, expr:TokenLiteral())
	)
end

---@param expr Expression
---@param value string -- The expected identifier
local function testIdentifier(expr, value)
	assert.are_equal("Identifier", typeof(expr),
		string.format("Expected expression to be an Identifier, got %s", typeof(expr))
	)

	assert.are_equal(value, expr.Value,
		string.format("Expected expression.Value to be %s, got %s", value, expr.Value)
	)

	assert.are_equal(value, expr:TokenLiteral(),
		string.format("Expected expression::TokenLiteral to be '%s', got %s", value, expr:TokenLiteral())
	)
end

---@param expr Expression
---@param value string -- The expected identifier
local function testBooleanLiteral(expr, value)
	assert.are_equal("BooleanLiteral", typeof(expr),
		string.format("Expected expression to be an BooleanLiteral, got %s", typeof(expr))
	)

	assert.are_equal(value, expr.Value,
		string.format("Expected expression.Value to be %s, got %s", value, expr.Value)
	)

	assert.are_equal(tostring(value), expr:TokenLiteral(),
		string.format("Expected expression::TokenLiteral to be '%s', got %s", value, expr:TokenLiteral())
	)
end

local function TestLiteralExpression(expr, expected)
	local typ = type(expected)

	if typ == "number" then
		testIntegerLiteral(expr, expected)
	elseif typ == "string" then
		testIdentifier(expr, expected)
	elseif typ == "boolean" then
		testBooleanLiteral(expr, expected)
	end
end

---@param expr any
---@param left any
---@param operator string
---@param right any
---@return boolean
local function TestInfixExpression(expr, left, operator, right)
	assert.are_equal("InfixExpression", typeof(expr),
		string.format("Expected statement.Expression to be an InfixExpression, got %s", typeof(expr))
	)

	---@cast expr InfixExpression

	TestLiteralExpression(expr.Left, left)

	assert.are_equal(operator, expr.Operator,
		string.format("Expected statement.Expression.Operator to be %s, got %s", operator, expr.Operator)
	)

	TestLiteralExpression(expr.Right, right)
end

describe("the parser", function()
	it("can do let statements", function()
		local inputs = {
			{ "let x = 5;",      "x",      5 },
			{ "let y = 10;",     "y",      10 },
			{ "let foobar = y;", "foobar", "y" }
		}

		for _, test in ipairs(inputs) do
			local lex = lexer.new(test[1])
			local parser = parser.new(lex)
			local program = parser:ParseProgram()

			assert.is.truthy(program, "Invalid Program!")
			assert.are_equal(1, #program.Statements, "Expected 1 statement!")

			local stmt = program.Statements[1]
			testLetStatement(stmt, test[2])

			---@cast stmt LetStatement

			TestLiteralExpression(stmt.Value, test[3])
		end
	end)

	it("can detect incorrect let syntax", function()
		local input = [[
			let x 5
			let 838383;
		]]

		local lex = lexer.new(input)
		local parser = parser.new(lex)
		parser:ParseProgram()

		assert.are_equal(2, #parser.errors, "Expected 2 errors, got " .. #parser.errors .. "!")
	end)

	it("can handle returns", function()
		local input = [[
			return 5;
			return 10;
			return 993322;
		]]

		local lex = lexer.new(input)
		local parser = parser.new(lex)
		local program = parser:ParseProgram()

		assert.is.truthy(program, "Invalid Program!")
		assert.are_equal(3, #program.Statements, "Expected 3 statements!")

		---@type Statement
		for _, statement in ipairs(program.Statements) do
			assert.are_equal(statement:TokenLiteral(), "return",
				"TokenLiteral not 'return', got " .. statement:TokenLiteral())
		end
	end)

	it("can produce a basic AST", function()
		local program = ast.Program.new()
		program.Statements[1] = ast.LetStatement.new(
			{ Type = tokens.LET, Literal = 'let' },
			ast.Identifier.new(
				{ Type = tokens.IDENT, Literal = 'myVar' },
				"myVar"
			),
			ast.Identifier.new(
				{ Type = tokens.IDENT, Literal = 'anotherVar' },
				'anotherVar'
			)
		)

		assert.are_equal("let myVar = anotherVar;", tostring(program), "Could not create valid AST!")
	end)

	it("can do identifier expressions", function()
		local input = "foobar;"

		local lex = lexer.new(input)
		local parser = parser.new(lex)
		local program = parser:ParseProgram()

		assert.are_equal("Program", typeof(program),
			string.format("Expected parser::ParseProgram to return a Program, got %s", typeof(program))
		)

		assert.are_equal(1, #program.Statements, "Expected 1 statement!")
		assert.are_equal("ExpressionStatement", typeof(program.Statements[1]),
			string.format("Expected statement to be an ExpressionStatement, got %s", typeof(program.Statements[1]))
		)
		local statement = program.Statements[1]
		TestLiteralExpression(statement.Expression, "foobar")
	end)

	it("can do integer literals", function()
		local input = "5;"

		local lex = lexer.new(input)
		local parser = parser.new(lex)
		local program = parser:ParseProgram()

		assert.are_equal("Program", typeof(program),
			string.format("Expected parser::ParseProgram to return a Program, got %s", typeof(program))
		)

		assert.are_equal(1, #program.Statements, "Expected 1 statement!")
		assert.are_equal("ExpressionStatement", typeof(program.Statements[1]),
			string.format("Expected statement to be an ExpressionStatement, got %s", typeof(program.Statements[1]))
		)

		TestLiteralExpression(program.Statements[1].Expression, 5)
	end)

	it("can do prefix expressions", function()
		local tests = {
			{ "!5;",    "!", 5 },
			{ "-15",    "-", 15 },
			{ "!true",  "!", true },
			{ "!false", "!", false }
		}

		for _, test in ipairs(tests) do
			local lex = lexer.new(test[1])
			local parser = parser.new(lex)
			local program = parser:ParseProgram()

			assert.are_equal(1, #program.Statements,
				"Expected 1 statement! \nErrors: \n  " .. tostring(parser.errors) .. "\n")
			assert.are_equal("ExpressionStatement", typeof(program.Statements[1]),
				string.format("Expected statement to be an ExpressionStatement, got %s", typeof(program.Statements[1]))
			)

			local statement = program.Statements[1]
			---@cast statement ExpressionStatement

			assert.are_equal("PrefixExpression", typeof(statement.Expression),
				string.format("Expected statement.Expression to be an PrefixExpression, got %s",
					typeof(statement.Expression))
			)

			local expr = statement.Expression
			---@cast expr PrefixExpression

			assert.are_equal(test[2], expr.Operator,
				string.format("Expected statement.Expression.Operator to be %s, got %s", test[2], expr.Operator)
			)

			TestLiteralExpression(expr.Right, test[3])
		end
	end)

	it("can do basic infix expressions", function()
		local tests = {
			{ "5 + 5;",         5,     "+",  5 },
			{ "5 - 5;",         5,     "-",  5 },
			{ "5 * 5;",         5,     "*",  5 },
			{ "5 / 5;",         5,     "/",  5 },
			{ "5 > 5;",         5,     ">",  5 },
			{ "5 < 5;",         5,     "<",  5 },
			{ "5 == 5;",        5,     "==", 5 },
			{ "5 != 5;",        5,     "!=", 5 },
			{ "true == true",   true,  "==", true },
			{ "true != false",  true,  "!=", false },
			{ "false == false", false, "==", false }

		}

		for _, test in ipairs(tests) do
			local lex = lexer.new(test[1])
			local parser = parser.new(lex)
			local program = parser:ParseProgram()

			assert.are_equal(1, #program.Statements, string.format(
				"Expected 1 statement, got %s! \nErrors: \n  %s\nProgram: \n%s\n",
				#program.Statements,
				tostring(parser.errors),
				tostring(program)
			))

			assert.are_equal("ExpressionStatement", typeof(program.Statements[1]),
				string.format("Expected statement to be an ExpressionStatement, got %s", typeof(program.Statements[1]))
			)

			local statement = program.Statements[1]
			---@cast statement ExpressionStatement

			TestInfixExpression(statement.Expression, test[2], test[3], test[4])
		end
	end)

	--TestOperatorPrecedenceParsing
	it("can do advanced infix expressions", function()
		local tests = {
			{ "-a * b",                                    "((-a) * b)", },
			{ "!-a",                                       "(!(-a))", },
			{ "a + b + c",                                 "((a + b) + c)", },
			{ "a + b - c",                                 "((a + b) - c)", },
			{ "a * b * c",                                 "((a * b) * c)", },
			{ "a * b / c",                                 "((a * b) / c)", },
			{ "a + b / c",                                 "(a + (b / c))", },
			{ "a + b * c + d / e - f",                     "(((a + (b * c)) + (d / e)) - f)", },
			{ "3 + 4; -5 * 5",                             "(3 + 4)((-5) * 5)", },
			{ "5 > 4 == 3 < 4",                            "((5 > 4) == (3 < 4))", },
			{ "5 < 4 != 3 > 4",                            "((5 < 4) != (3 > 4))", },
			{ "3 + 4 * 5 == 3 * 1 + 4 * 5",                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))", },
			{ "3 + 4 * 5 == 3 * 1 + 4 * 5",                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))", },
			{ "true",                                      "true" },
			{ "false",                                     "false" },
			{ "3 > 5 == false",                            "((3 > 5) == false)" },
			{ "3 < 5 == true",                             "((3 < 5) == true)" },
			{ "1 + (2 + 3) + 4",                           "((1 + (2 + 3)) + 4)" },
			{ "(5 + 5) * 2",                               "((5 + 5) * 2)" },
			{ "2 / (5 + 5)",                               "(2 / (5 + 5))" },
			{ "-(5 + 5)",                                  "(-(5 + 5))" },
			{ "!(true == true)",                           "(!(true == true))" },
			{ "a + add(b * c) + d",                        "((a + add((b * c))) + d)" },
			{ "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))" },
			{ "add(a + b + c * d / f + g)",                "add((((a + b) + ((c * d) / f)) + g))" }
		}

		for _, test in ipairs(tests) do
			local lex = lexer.new(test[1])
			local parser = parser.new(lex)
			local program = parser:ParseProgram()
			assert.are_equal(0, #parser.errors, string.format(
				'Expected 0 errors, got %s\nErrors:\n%s\n',
				#parser.errors,
				tostring(parser.errors)
			))

			assert.are_equal(test[2], tostring(program), "Incorrect output")
		end
	end)

	it("can do if statements", function()
		local input = [[
			if (x < y) { x }
		]]

		local lex = lexer.new(input)
		local parser = parser.new(lex)
		local program = parser:ParseProgram()

		assert.are_equal(0, #parser.errors, string.format(
			'Expected 0 errors, got %s!\nErrors:\n  %s\n',
			#parser.errors,
			tostring(parser.errors)
		))
		assert.are_equal(1, #program.Statements, "Expected 1 program statement!")


		local stmt = program.Statements[1]
		expect("ExpressionStatement", stmt)

		---@cast stmt ExpressionStatement
		local expr = stmt.Expression
		expect("IfExpression", expr)

		---@cast expr IfExpression
		TestInfixExpression(expr.Condition, "x", "<", "y")

		assert.are_equal(1, #expr.Consequence.Statements, "Expected 1 consequence statement!")

		local consequence = expr.Consequence.Statements[1]
		expect("ExpressionStatement", consequence)

		---@cast consequence ExpressionStatement
		testIdentifier(consequence.Expression, "x")
	end)

	it("can do if else statements", function()
		local input = [[
			if (x < y) { x } else { y }
		]]

		local lex = lexer.new(input)
		local parser = parser.new(lex)
		local program = parser:ParseProgram()

		assert.are_equal(0, #parser.errors, string.format(
			'Expected 0 errors, got %s!\nErrors:\n  %s\n',
			#parser.errors,
			tostring(parser.errors)
		))
		assert.are_equal(1, #program.Statements, "Expected 1 program statement!")


		local stmt = program.Statements[1]
		expect("ExpressionStatement", stmt)

		---@cast stmt ExpressionStatement
		local expr = stmt.Expression
		expect("IfExpression", expr)

		---@cast expr IfExpression
		TestInfixExpression(expr.Condition, "x", "<", "y")

		assert.are_equal(1, #expr.Consequence.Statements, "Expected 1 consequence statement!")

		local consequence = expr.Consequence.Statements[1]
		expect("ExpressionStatement", consequence)

		---@cast consequence ExpressionStatement
		testIdentifier(consequence.Expression, "x")
	end)

	it("can do literal function parsing", function()
		local input = "fn(x, y) { x + y; }"

		local lex = lexer.new(input)
		local parser = parser.new(lex)
		local program = parser:ParseProgram()

		assert.are_equal(0, #parser.errors, string.format(
			'Expected 0 errors, got %s!\nErrors:\n  %s\n',
			#parser.errors,
			tostring(parser.errors)
		))
		assert.are_equal(1, #program.Statements, "Expected 1 program statement!")

		local exprStmt = program.Statements[1]
		expect("ExpressionStatement", exprStmt)

		---@cast exprStmt ExpressionStatement
		local fnLiteral = exprStmt.Expression
		expect("FunctionLiteral", fnLiteral)

		---@cast fnLiteral FunctionLiteral

		assert.are_equal(2, #fnLiteral.Parameters, "Expected 2 function parameters!")
		TestLiteralExpression(fnLiteral.Parameters[1], "x")
		TestLiteralExpression(fnLiteral.Parameters[2], "y")

		local body = fnLiteral.Body.Statements
		assert.are_equal(1, #body, "Expected 1 function body statement!")

		local bodyStmt = body[1]
		expect("ExpressionStatement", bodyStmt)

		---@cast bodyStmt ExpressionStatement
		TestInfixExpression(bodyStmt.Expression, "x", "+", "y")
	end)

	it("can parse function parameters", function()
		local inputs = {
			{ input = "fn() {}",        expectedParams = {} },
			{ input = "fn(x) {}",       expectedParams = { "x" } },
			{ input = "fn(x, y, z) {}", expectedParams = { "x", "y", "z" } },
		}

		for _, test in ipairs(inputs) do
			local lex = lexer.new(test.input)
			local parser = parser.new(lex)
			local program = parser:ParseProgram()

			assert.are_equal(0, #parser.errors, string.format(
				'Expected 0 errors, got %s!\nErrors:\n  %s\n',
				#parser.errors,
				tostring(parser.errors)
			))

			local exprStmt = program.Statements[1]
			expect("ExpressionStatement", exprStmt)

			---@cast exprStmt ExpressionStatement
			local fnLiteral = exprStmt.Expression
			expect("FunctionLiteral", fnLiteral)

			assert.are_equal(#test.expectedParams, #fnLiteral.Parameters, string.format(
				"Expected %s function parameters, got %s!",
				#test.expectedParams,
				#fnLiteral.Parameters
			))

			for i, ident in ipairs(test.expectedParams) do
				TestLiteralExpression(fnLiteral.Parameters[i], ident)
			end
		end
	end)

	it("can do call expressions", function()
		local input = "add(1, 2 * 3, 4 + 5);"

		local lex = lexer.new(input)
		local parser = parser.new(lex)
		local program = parser:ParseProgram()

		assert.are_equal(0, #parser.errors, string.format(
			'Expected 0 errors, got %s!\nErrors:\n  %s\n',
			#parser.errors,
			tostring(parser.errors)
		))
		assert.are_equal(1, #program.Statements, "Expected 1 program statement!")

		local exprStmt = program.Statements[1]
		expect("ExpressionStatement", exprStmt)

		---@cast exprStmt ExpressionStatement
		local callExpr = exprStmt.Expression
		expect("CallExpression", callExpr)

		---@cast callExpr CallExpression
		testIdentifier(callExpr.Function, "add")

		assert.are_equal(3, #callExpr.Arguments, string.format(
			"Expected 3 function parameters, got %s!",
			#callExpr.Arguments
		))

		TestLiteralExpression(callExpr.Arguments[1], 1)
		TestInfixExpression(callExpr.Arguments[2], 2, "*", 3)
		TestInfixExpression(callExpr.Arguments[3], 4, "+", 5)
	end)

	it("can do strings", function()
		local input = '"Hello, world!"'

		local lex = lexer.new(input)
		local parser = parser.new(lex)
		local program = parser:ParseProgram()

		assert.are_equal(0, #parser.errors, string.format(
			'Expected 0 errors, got %s!\nErrors:\n  %s\n',
			#parser.errors,
			tostring(parser.errors)
		))

		local stmt = program.Statements[1]

		expect("ExpressionStatement", stmt)
		---@cast stmt ExpressionStatement
		
		local expr = stmt.Expression

		expect("StringLiteral", expr)
		---@cast expr StringLiteral
		
		assert.are_equal("Hello, world!", expr.Value, string.format(
			"Expected string.Value to be %s, got %s",
			input,
			expr.Value
		))
	end)
end)
