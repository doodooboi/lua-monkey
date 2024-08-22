---@diagnostic disable: undefined-global, undefined-field
local tokens = require("components.tokens")
local lexer = require("components.lexer")

describe("the lexer", function()
	it("can do basic symbols", function()
		local input = "=+(){},;"
		local expects = {
			{ tokens.ASSIGN,    "=" },
			{ tokens.PLUS,      "+" },
			{ tokens.LPAREN,    "(" },
			{ tokens.RPAREN,    ")" },
			{ tokens.LBRACE,    "{" },
			{ tokens.RBRACE,    "}" },
			{ tokens.COMMA,     "," },
			{ tokens.SEMICOLON, ";" },
			{ tokens.EOF,       "" }
		}

		local tempLexer = lexer.new(input)
		for index, test in ipairs(expects) do
			local token = tempLexer:nextToken()
			assert.are.same({ token.Type, token.Literal }, test)
		end
	end)

	it("can do a basic program", function()
		local input = [[let five = 5;
    let ten = 10;

    let add = fn(x, y) {
      x + y;
    };

    let result = add(five, ten);
    ]]
		local expects = {
			{ tokens.LET,       "let" },
			{ tokens.IDENT,     "five" },
			{ tokens.ASSIGN,    "=" },
			{ tokens.INT,       "5" },
			{ tokens.SEMICOLON, ";" },
			{ tokens.LET,       "let" },
			{ tokens.IDENT,     "ten" },
			{ tokens.ASSIGN,    "=" },
			{ tokens.INT,       "10" },
			{ tokens.SEMICOLON, ";" },
			{ tokens.LET,       "let" },
			{ tokens.IDENT,     "add" },
			{ tokens.ASSIGN,    "=" },
			{ tokens.FUNCTION,  "fn" },
			{ tokens.LPAREN,    "(" },
			{ tokens.IDENT,     "x" },
			{ tokens.COMMA,     "," },
			{ tokens.IDENT,     "y" },
			{ tokens.RPAREN,    ")" },
			{ tokens.LBRACE,    "{" },
			{ tokens.IDENT,     "x" },
			{ tokens.PLUS,      "+" },
			{ tokens.IDENT,     "y" },
			{ tokens.SEMICOLON, ";" },
			{ tokens.RBRACE,    "}" },
			{ tokens.SEMICOLON, ";" },
			{ tokens.LET,       "let" },
			{ tokens.IDENT,     "result" },
			{ tokens.ASSIGN,    "=" },
			{ tokens.IDENT,     "add" },
			{ tokens.LPAREN,    "(" },
			{ tokens.IDENT,     "five" },
			{ tokens.COMMA,     "," },
			{ tokens.IDENT,     "ten" },
			{ tokens.RPAREN,    ")" },
			{ tokens.SEMICOLON, ";" },
			{ tokens.EOF,       "" },
		}

		local tempLexer = lexer.new(input)
		for index, test in ipairs(expects) do
			local token = tempLexer:nextToken()
			assert.are.same({ token.Type, token.Literal }, test)
		end
	end)

	it("can do operators", function()
		local input = [[
      !-/*5;
      5 < 10 > 5;
		]]
		local expects = {
			{ tokens.BANG,      "!" },
			{ tokens.MINUS,     "-" },
			{ tokens.DIV,       "/" },
			{ tokens.MULT,      "*" },
			{ tokens.INT,       "5" },
			{ tokens.SEMICOLON, ";" },
			{ tokens.INT,       "5" },
			{ tokens.LT,        "<" },
			{ tokens.INT,       "10" },
			{ tokens.GT,        ">" },
			{ tokens.INT,       "5" },
			{ tokens.SEMICOLON, ";" }
		}

		local tempLexer = lexer.new(input)
		for index, test in ipairs(expects) do
			local token = tempLexer:nextToken()
			assert.are.same({ token.Type, token.Literal }, test)
		end
	end)

	it("can do a basic if statement", function()
		local input = [[
    if (5 < 10) {
      return true;
    } else {
      return false;
    }
    ]]
		local expects = {
			{ tokens.IF,        "if" },
			{ tokens.LPAREN,    "(" },
			{ tokens.INT,       "5" },
			{ tokens.LT,        "<" },
			{ tokens.INT,       "10" },
			{ tokens.RPAREN,    ")" },
			{ tokens.LBRACE,    "{" },
			{ tokens.RETURN,    "return" },
			{ tokens.TRUE,      "true" },
			{ tokens.SEMICOLON, ";" },
			{ tokens.RBRACE,    "}" },
			{ tokens.ELSE,      "else" },
			{ tokens.LBRACE,    "{" },
			{ tokens.RETURN,    "return" },
			{ tokens.FALSE,     "false" },
			{ tokens.SEMICOLON, ";" },
			{ tokens.RBRACE,    "}" }
		}

		local tempLexer = lexer.new(input)
		for index, test in ipairs(expects) do
			local token = tempLexer:nextToken()
			assert.are.same({ token.Type, token.Literal }, test)
		end
	end)

	it("can do a equality check", function()
		local input = [[
    10 == 10;
    10 != 9;
	"foobar"
	" foo  bar"
	"I have nested quotes! \"omggg\""
	[1, 2];
	{"foo": "bar"}
    ]]
		local expects = {
			{ tokens.INT,       "10" },
			{ tokens.EQ,        "==" },
			{ tokens.INT,       "10" },
			{ tokens.SEMICOLON, ";" },
			{ tokens.INT,       "10" },
			{ tokens.NOT_EQ,    "!=" },
			{ tokens.INT,       "9" },
			{ tokens.SEMICOLON, ";" },
			{ tokens.STRING,    "foobar" },
			{ tokens.STRING,    " foo  bar" },
			{ tokens.STRING,    'I have nested quotes! "omggg"' },
			{ tokens.LBRACKET,   "[" },
			{ tokens.INT,        "1" },
			{ tokens.COMMA,      "," },
			{ tokens.INT,        "2" },
			{ tokens.RBRACKET,   "]" },
			{ tokens.SEMICOLON,  ";" },
			{ tokens.LBRACE, '{'},
			{ tokens.STRING, "foo"},
			{ tokens.COLON, ":"},
			{ tokens.STRING, "bar"},
			{ tokens.RBRACE, "}"},
			{ tokens.EOF, ""}
		}

		local tempLexer = lexer.new(input)
		for _, test in ipairs(expects) do
			local token = tempLexer:nextToken()

			assert.are.same(test, { token.Type, token.Literal })
		end
	end)
end)
