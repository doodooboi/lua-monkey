local oo = require("utility.oo")
local ast = require("components.ast")
local tokens = require("components.tokens")

require("utility.utility")

---@enum constant
local constants = {
	LOWEST = 0x0,
	EQUALS = 0x1,
	LESSGREATER = 0x2,
	SUM = 0x3,
	PRODUCT = 0x4,
	PREFIX = 0x5,
	CALL = 0x6,
	INDEX = 0x7,
}

---@enum precedence
local precedences = {
	[tokens.EQ] = constants.EQUALS,
	[tokens.NOT_EQ] = constants.EQUALS,

	[tokens.LT] = constants.LESSGREATER,
	[tokens.GT] = constants.LESSGREATER,

	[tokens.PLUS] = constants.SUM,
	[tokens.MINUS] = constants.SUM,

	[tokens.DIV] = constants.PRODUCT,
	[tokens.MULT] = constants.PRODUCT,

	[tokens.LPAREN] = constants.CALL,
	[tokens.LBRACKET] = constants.INDEX
}

---@generic K, V
---@alias map {[K]: V}

---@alias prefixParseFn fun(): Expression
---@alias infixParseFn fun(expr: Expression): Expression

-- TODO: Update types

---@class Parser
---@field lexer lexer
---@field curToken token
---@field peekToken token
---@field errors string[]
---@field nextToken fun(self: Parser): nil
---@field ParseProgram fun(self: Parser): Program
---@field registerPrefix fun(self: Parser, tokenType: string, fn: prefixParseFn)
---@field registerInfix fun(self: Parser, tokenType: string, fn: infixParseFn)
---@field private prefixParseFns map<string, prefixParseFn>
---@field private infixParseFns map<string, infixParseFn>
---@field private parseStatement fun(self: Parser): Statement?
---@field private parseLetStatement fun(self: Parser): LetStatement?
---@field private parseReturnStatement fun(self: Parser): ReturnStatement?
---@field private parseExpressionStatement fun(self: Parser): ExpressionStatement?
---@field private parseExpression fun(self: Parser, precedence: constant): Expression?
---@field private parseIdentifier fun(self: Parser): Expression?
---@field private parseIntegerLiteral fun(self: Parser): IntegerLiteral?
---@field private parsePrefixExpression fun(self: Parser): PrefixExpression?
---@field private parseInfixExpression fun(self: Parser, left: Expression): Expression?
---@field private expectPeek fun(self: Parser, token: string): boolean
---@field private peekError fun(self: Parser, token: string): nil
---@field private parseBoolean fun(self: Parser): BooleanLiteral?
---@field private peekPrecedence fun(self: Parser): number
---@field private curPrecedence fun(self: Parser): number
---@field private parseExpressionList fun(self: Parser, endTokenType: string): Expression[]
---@field private parseArrayIndexAssignmentStatement fun(self: Parser, curToken: token, identifier: Identifier, Index: Expression)
---@field new fun(l: lexer): Parser
local parser = oo.class()

---@param l lexer
function parser:init(l)
	self.lexer = l

	self.errors = setmetatable({}, {
		__tostring = function(t)
			return table.concat(t, "\n  ")
		end
	})

	self.prefixParseFns = {}
	self.infixParseFns = {}

	self:registerPrefix(tokens.IDENT, self.parseIdentifier)
	self:registerPrefix(tokens.INT, self.parseIntegerLiteral)
	self:registerPrefix(tokens.BANG, self.parsePrefixExpression)
	self:registerPrefix(tokens.MINUS, self.parsePrefixExpression)

	self:registerPrefix(tokens.IF, self.parseIfExpression)
	self:registerPrefix(tokens.FUNCTION, self.parseFunctionLiteral)

	self:registerPrefix(tokens.TRUE, self.parseBoolean)
	self:registerPrefix(tokens.FALSE, self.parseBoolean)
	self:registerPrefix(tokens.STRING, self.parseStringLiteral)
	self:registerPrefix(tokens.LBRACKET, self.parseArrayLiteral)
	self:registerPrefix(tokens.LBRACE, self.parseHashLiteral)

	self:registerPrefix(tokens.LPAREN, self.parseGroupedExpression)

	for precedence, _ in pairs(precedences) do
		self:registerInfix(precedence, self.parseInfixExpression)
	end

	self:registerInfix(tokens.LPAREN, self.parseCallExpression)
	self:registerInfix(tokens.LBRACKET, self.parseIndexExpression)

	-- read 2 tokens so curToken and peekToken are set
	self:nextToken()
	self:nextToken()
end

function parser:nextToken()
	self.curToken = self.peekToken
	self.peekToken = self.lexer:nextToken()
end

function parser:peekPrecedence()
	return precedences[self.peekToken.Type] or constants.LOWEST
end

function parser:curPrecedence()
	return precedences[self.curToken.Type] or constants.LOWEST
end

function parser:parseBoolean()
	return ast.Boolean.new(self.curToken, self.curToken.Type == tokens.TRUE)
end

function parser:parseIfExpression()
	local token = self.curToken
	if not self:expectPeek(tokens.LPAREN) then return end

	self:nextToken()
	local condition = self:parseExpression(constants.LOWEST)
	if not condition then return end

	if not self:expectPeek(tokens.RPAREN) then return end
	if not self:expectPeek(tokens.LBRACE) then return end

	local consequence = self:parseBlockStatement()
	local alt = nil

	if self.peekToken.Type == tokens.ELSE then
		self:nextToken() -- skip the else

		if not self:expectPeek(tokens.LBRACE) then return end
		alt = self:parseBlockStatement()
	end

	return IfExpression.new(token, condition, consequence, alt)
end

function parser:parseFunctionLiteral()
	local token = self.curToken

	if not self:expectPeek(tokens.LPAREN) then return end
	local params = self:parseFunctionParameters()

	if not self:expectPeek(tokens.LBRACE) then return end

	return FunctionLiteral.new(token, params, self:parseBlockStatement())
end

function parser:parseStringLiteral()
	return StringLiteral.new(self.curToken, self.curToken.Literal)
end

function parser:parseCallExpression(fun)
	local token = self.curToken

	return CallExpression.new(token, fun, self:parseExpressionList(tokens.RPAREN))
end

function parser:parseArrayLiteral()
	local token = self.curToken

	return ast.ArrayLiteral.new(token, self:parseExpressionList(tokens.RBRACKET))
end

function parser:parseIndexExpression(left)
	local curToken = self.curToken
	self:nextToken()

	local index = self:parseExpression(constants.LOWEST)
	if not self:expectPeek(tokens.RBRACKET) or not index then
		return nil
	end

	if self.peekToken.Type == tokens.ASSIGN then
		self:nextToken()
		return self:parseArrayIndexAssignmentStatement(curToken, left, index)
	end

	return ast.IndexExpression.new(curToken, left, index)
end

function parser:parseHashLiteral() 
	local curToken = self.curToken
	local pairs = {}

	while self.peekToken.Type ~= tokens.RBRACE do
		self:nextToken()
		-- print("start", self.curToken)

		local key = self:parseExpression(constants.LOWEST)

		if not self:expectPeek(tokens.COLON) then
			return
		end

		self:nextToken()
		local value = self:parseExpression(constants.LOWEST)
		-- print(string.format("{[%s(%s)]: %s(%s)}", tostring(key), typeof(key), tostring(value), typeof(value)))
		if key == nil or value == nil then return end

		pairs[key] = value
		-- print("peeking", self.peekToken)
		if self.peekToken.Type ~= tokens.RBRACE and self.peekToken.Type ~= tokens.COMMA then
			return
		end

		if self.peekToken.Type == tokens.COMMA then
			self:nextToken()
		end
		-- print("nxt loop")
	end

	if not self:expectPeek(tokens.RBRACE) then
		return
	end



	return ast.HashLiteral.new(curToken, pairs)
end

function parser:parseArrayIndexAssignmentStatement(curToken, identifier, index)
	self:nextToken()

	local value = self:parseExpression(constants.LOWEST)
	if not value then return end

	return ast.ArrayIndexExpression.new(curToken, identifier, index, value)
end

function parser:parseExpressionList(endTokenType)
	local args = {}

	if self.peekToken.Type == endTokenType then
		self:nextToken()
		return args
	end

	self:nextToken()
	table.insert(args, self:parseExpression(constants.LOWEST))

	while self.peekToken.Type == tokens.COMMA do
		self:nextToken()
		self:nextToken()

		table.insert(args, self:parseExpression(constants.LOWEST))
	end

	if not self:expectPeek(endTokenType) then
		return
	end

	return args
end

function parser:parseCallArguments()
	local args = {}

	if self.peekToken.Type == tokens.RPAREN then
		self:nextToken()
		return args
	end

	self:nextToken()
	table.insert(args, self:parseExpression(constants.LOWEST))

	while self.peekToken.Type == tokens.COMMA do
		self:nextToken()
		self:nextToken()

		table.insert(args, self:parseExpression(constants.LOWEST))
	end

	if not self:expectPeek(tokens.RPAREN) then
		return
	end

	return args
end

function parser:parseFunctionParameters()
	local identifiers = {}

	if self.peekToken.Type == tokens.RPAREN then
		self:nextToken()
		return identifiers
	end

	self:nextToken()
	local ident = Identifier.new(self.curToken, self.curToken.Literal)
	table.insert(identifiers, ident)

	while self.peekToken.Type == tokens.COMMA do
		self:nextToken()
		self:nextToken()

		local ident = Identifier.new(self.curToken, self.curToken.Literal)
		table.insert(identifiers, ident)
	end

	if not self:expectPeek(tokens.RPAREN) then
		return
	end

	return identifiers
end

function parser:parseBlockStatement()
	local block = BlockStatement.new(self.curToken)
	self:nextToken()

	while self.curToken.Type ~= tokens.RBRACE and self.curToken.Type ~= tokens.EOF do
		local statement = self:parseStatement()

		if statement then
			table.insert(block.Statements, statement)
		end

		self:nextToken()
	end

	return block
end

function parser:parseStatement()
	if self.curToken.Type == tokens.LET then
		return self:parseLetStatement()
	elseif self.curToken.Type == tokens.RETURN then
		return self:parseReturnStatement()
	else
		if self.peekToken.Type == tokens.ASSIGN then
			return self:parseAssignmentStatement()
		end

		return self:parseExpressionStatement()
	end
end

function parser:parseAssignmentStatement()
	local identToken = self.curToken
	if not self:expectPeek(tokens.ASSIGN) then return end

	self:nextToken()
	local value = self:parseExpression(constants.LOWEST)

	while self.curToken.Type ~= tokens.SEMICOLON and self.curToken.Type ~= tokens.EOF do
		self:nextToken()
	end

	local identifier = ast.Identifier.new(identToken, identToken.Literal)

	---@diagnostic disable-next-line: param-type-mismatch
	return AssignmentStatement.new(identToken, identifier, value)
end

function parser:parseLetStatement()
	local startToken = self.curToken -- The LET token
	local const = self.peekToken.Type == tokens.CONSTANT

	if const then
		self:nextToken()
	end

	if not self:expectPeek(tokens.IDENT) then return end

	local identToken = self.curToken
	if not self:expectPeek(tokens.ASSIGN) then return end

	self:nextToken()

	local value = self:parseExpression(constants.LOWEST)

	while self.curToken.Type ~= tokens.SEMICOLON and self.curToken.Type ~= tokens.EOF do
		self:nextToken()
	end

	local identifier = ast.Identifier.new(identToken, identToken.Literal)

	---@diagnostic disable-next-line: param-type-mismatch
	return ast.LetStatement.new(startToken, identifier, value, const or false)
end

function parser:parseGroupedExpression()
	self:nextToken()

	local expr = self:parseExpression(constants.LOWEST)

	if not self:expectPeek(tokens.RPAREN) then
		return nil
	end

	return expr
end

function parser:parseReturnStatement()
	local startToken = self.curToken -- the RETURN token
	self:nextToken()

	local returnValue = self:parseExpression(constants.LOWEST)

	while self.curToken.Type ~= tokens.SEMICOLON and self.curToken.Type ~= tokens.EOF do
		self:nextToken()
	end

	return ast.ReturnStatement.new(startToken, returnValue)
end

function parser:parseExpressionStatement()
	local startToken = self.curToken
	local expr = self:parseExpression(constants.LOWEST)
	local exprStatement = ast.ExpressionStatement.new(startToken, expr)

	if self.peekToken.Type == tokens.SEMICOLON then
		self:nextToken()
	end

	return exprStatement
end

function parser:parseIntegerLiteral()
	local int = tonumber(self.curToken.Literal)
	if not int then
		table.insert(self.errors, string.format(
			"Could not parse %s as integer",
			self.curToken.Literal
		))
		return
	end

	return ast.IntegerLiteral.new(self.curToken, int)
end

function parser:parseExpression(precedence)
	---@type prefixParseFn
	local fn = self.prefixParseFns[self.curToken.Type]

	if not fn then
		self:noPrefixParseFnError(self.curToken.Type)
		return
	end

	---@diagnostic disable-next-line: redundant-parameter
	local left = fn(self)

	while self.peekToken.Type ~= tokens.SEMICOLON and precedence < self:peekPrecedence() do
		---@type infixParseFn
		local infix = self.infixParseFns[self.peekToken.Type]

		if not infix then
			return left
		end

		self:nextToken()

		---@diagnostic disable-next-line: param-type-mismatch, redundant-parameter
		left = infix(self, left)
	end

	return left
end

function parser:parsePrefixExpression()
	local startToken = self.curToken
	self:nextToken()

	return ast.PrefixExpression.new(startToken, startToken.Literal, self:parseExpression(constants.PREFIX))
end

function parser:parseInfixExpression(left)
	local startToken = self.curToken
	local precedence = self:curPrecedence()
	self:nextToken()

	return ast.InfixExpression.new(startToken, startToken.Literal, left, self:parseExpression(precedence))
end

function parser:parseIdentifier()
	return ast.Identifier.new(self.curToken, self.curToken.Literal)
end

function parser:expectPeek(token)
	if self.peekToken.Type == token then
		self:nextToken()
		return true
	else
		self:peekError(token)
		return false
	end
end

function parser:peekError(token)
	table.insert(self.errors, string.format(
		'Expected next token to be %s, got %s',
		token, self.peekToken.Type
	))
end

function parser:noPrefixParseFnError(token)
	table.insert(self.errors, string.format(
		"No prefix parse function for %s found",
		token
	))
end

function parser:registerPrefix(token, fn)
	self.prefixParseFns[token] = fn
end

function parser:registerInfix(token, fn)
	self.infixParseFns[token] = fn
end

function parser:ParseProgram()
	local program = ast.Program.new()

	while true do
		if self.curToken.Type == tokens.EOF then break end

		local statement = self:parseStatement()
		if statement then
			table.insert(program.Statements, statement)
		end

		self:nextToken()
	end

	return program
end

return parser
