local oo = require("oo")

-- Define the Node class
---@class Node
---@field TokenLiteral fun(self: Node): string
Node = oo.class()
function Node:init()
	-- Initialize the Node object
end

function Node:TokenLiteral()
	error("TokenLiteral() not implemented")
end

---@class Statement: Node
---@field statementNode fun(self: Statement)
Statement = oo.class(Node)
function Statement:init()
	Node.init(self)
end

function Statement:statementNode()
	error("statementNode() not implemented")
end
Statement.__metatable = "Statement"

---@class Expression: Node
---@field expressionNode fun(self: Expression)
---@field new fun(): Expression
Expression = oo.class(Node)
function Expression:init()
	Node.init(self)
end

function Expression:expressionNode()
	error("expressionNode() not implemented")
end
Expression.__metatable = "Expression"

---@class Identifier: Expression
---@field Token token -- IDENT
---@field Value string
---@field new fun(token: token, value: string): Identifier
Identifier = oo.class(Expression)

---@param token token
---@param value string
function Identifier:init(token, value)
	Expression.init(self)

	self.Token = token
	self.Value = value
end

function Identifier:expressionNode()
	return
end

function Identifier:TokenLiteral()
	return self.Token.Literal
end

function Identifier:__tostring()
	return self.Value
end
Identifier.__metatable = "Identifier"

---@class LetStatement: Statement
---@field Token token -- LET
---@field Name Identifier
---@field Value Expression
---@field new fun(token: token, name: Identifier, value: Expression)
LetStatement = oo.class(Statement)

---@param token token
---@param name Identifier
---@param value Expression
function LetStatement:init(token, name, value)
	Statement.init(self)

	self.Token = token
	self.Name = name
	self.Value = value
end

function LetStatement:TokenLiteral()
	return self.Token.Literal
end

function LetStatement:__tostring()
	return string.format(
		'%s %s = %s;',
		self:TokenLiteral(), -- LET
		tostring(self.Name), -- name
		tostring(self.Value or "?")
	)
end
LetStatement.__metatable = "LetStatement"

---@class ReturnStatement: Statement
---@field Token token -- RETURN
---@field ReturnValue Expression
---@field TokenLiteral fun(self: ReturnStatement): string
---@field new fun(token: token, returnValue: Expression?): ReturnStatement
ReturnStatement = oo.class(Statement)

---@param token token -- RETURN
---@param returnValue Expression
function ReturnStatement:init(token, returnValue)
	Statement.init(self)

	self.Token = token
	self.ReturnValue = returnValue
end

function ReturnStatement:statementNode()
	return
end

function ReturnStatement:TokenLiteral()
	return self.Token.Literal
end

function ReturnStatement:__tostring()
	return string.format(
		'%s %s;',
		self:TokenLiteral(),
		tostring(self.ReturnValue or "?")
	)
end
ReturnStatement.__metatable = "ReturnStatement"

---@class ExpressionStatement: Statement
---@field Token token -- RETURN
---@field Expression Expression
---@field TokenLiteral fun(self: ExpressionStatement): string
---@field statementNode fun(self: ExpressionStatement)
---@field new fun(token: token, returnValue: Expression?): ExpressionStatement
ExpressionStatement = oo.class(Statement)
function ExpressionStatement:init(token, returnValue)
	Statement.init(self)

	self.Token = token
	self.Expression = returnValue
end

function ExpressionStatement:statementNode()
	return
end

function ExpressionStatement:TokenLiteral()
	return self.Token.Literal
end

function ExpressionStatement:__tostring()
	return tostring(self.Expression or "?")
end
ExpressionStatement.__metatable = "ExpressionStatement"

---@class PrefixExpression: Expression
---@field Token token
---@field Operator string
---@field Right Expression
---@field new fun(token: token, operator: string, right: Expression?): PrefixExpression
PrefixExpression = oo.class(Expression)
function PrefixExpression:init(token, operator, right)
	Expression.init(self)

	self.Token = token
	self.Operator = operator
	self.Right = right
end

function PrefixExpression:TokenLiteral()
	return self.Token.Literal
end

function PrefixExpression:expressionNode()
	return
end

function PrefixExpression:__tostring()
	return string.format("(%s%s)", self.Operator, tostring(self.Right))
end
PrefixExpression.__metatable = "PrefixExpression"

---@class InfixExpression: Expression
---@field Token token
---@field Operator string
---@field Right Expression
---@field Left Expression
---@field new fun(token: token, operator: string, left: Expression?, right: Expression?): InfixExpression
InfixExpression = oo.class(Expression)
function InfixExpression:init(token, operator, left, right)
	Expression.init(self)

	self.Token = token
	self.Operator = operator
	self.Right = right
	self.Left = left
end

function InfixExpression:TokenLiteral()
	return self.Token.Literal
end

function InfixExpression:expressionNode()
	return
end

function InfixExpression:__tostring()
	return string.format("(%s %s %s)", tostring(self.Left), self.Operator, tostring(self.Right))
end
InfixExpression.__metatable = "InfixExpression"

---@class IntegerLiteral: Expression
---@field Token token
---@field Value number
---@field new fun(token: token, value: number): IntegerLiteral
IntegerLiteral = oo.class(Expression)
function IntegerLiteral:init(token, value)
	Expression.init(self)

	self.Token = token
	self.Value = value
end

function IntegerLiteral:TokenLiteral()
	return self.Token.Literal
end

function IntegerLiteral:__tostring()
	return self.Token.Literal
end
IntegerLiteral.__metatable = "IntegerLiteral"

---@class Boolean: Expression
---@field Token token
---@field Value boolean
---@field new fun(token: token, value: boolean): Boolean
Boolean = oo.class(Expression)
function Boolean:init(token, value)
	Expression.init(self)

	self.Token = token
	self.Value = value
end

function Boolean:expressionNode()
	return
end

function Boolean:TokenLiteral()
	return self.Token.Literal
end

function Boolean:__tostring()
	return self.Token.Literal
end
Boolean.__metatable = "Boolean"

---@class IfExpression: Expression
---@field Token token
---@field Condition Expression
---@field Consequence BlockStatement
---@field Alternative BlockStatement
---@field new fun(token: token, condition: Expression, consequence: BlockStatement, alternative: BlockStatement?): IfExpression
IfExpression = oo.class(Expression)

function IfExpression:init(token, condition, consequence, alternative)
	Expression.init(self)

	self.Token = token
	self.Condition = condition
	self.Consequence = consequence
	self.Alternative = alternative
end

function IfExpression:expressionNode()
	return
end

function IfExpression:TokenLiteral()
	return self.Token.Literal
end

function IfExpression:__tostring()
	local out = {'if ', tostring(self.Condition), " { ", tostring(self.Consequence)}

	if self.Alternative then
		table.insert(out, " } else { ")
		table.insert(out, tostring(self.Alternative))
	end
 
	table.insert(out, " }")

	return table.concat(out)
end
IfExpression.__metatable = "IfExpression"

---@class BlockStatement: Statement
---@field Token token
---@field Statements Statement[]
---@field new fun(token: token): BlockStatement
BlockStatement = oo.class(Statement)

function BlockStatement:init(token)
	Statement.init(self)

	self.Token = token
	self.Statements = {}
end

function BlockStatement:statementNode()
	return
end

function BlockStatement:TokenLiteral()
	return self.Token.Literal
end

function BlockStatement:__tostring()
	local out = {}

	for _, statement in ipairs(self.Statements) do
		table.insert(out, tostring(statement))
	end

	return table.concat(out)
end
BlockStatement.__metatable = "BlockStatement"

---@class FunctionLiteral: Expression
---@field Token token
---@field Parameters Identifier[]
---@field Body BlockStatement
---@field new fun(token: token, params: Identifier[]?, body: BlockStatement): FunctionLiteral
FunctionLiteral = oo.class(Expression)
function FunctionLiteral:init(token, params, body)
	Expression.init(self)

	self.Token = token
	self.Parameters = params or {}
	self.Body = body
end

function FunctionLiteral:TokenLiteral()
	return self.Token.Literal
end

function FunctionLiteral:expressionNode()
	return
end

function FunctionLiteral:__tostring()
	local params = {}

	for _, param in ipairs(self.Parameters) do
		table.insert(params, tostring(param))
	end

	return string.format(
		'%s(%s) %s',
		self:TokenLiteral(),
		table.concat(params, ', '),
		tostring(self.Body)
	)
end
FunctionLiteral.__metatable = "FunctionLiteral"

---@class CallExpression: Expression
---@field Token token
---@field Function Expression
---@field Arguments Expression[]
---@field new fun(token: token, fun: Expression, args: Expression[]): CallExpression
CallExpression = oo.class(Expression)

function CallExpression:init(token, fun, args)
	Expression.init(self)

	self.Token = token
	self.Function = fun
	self.Arguments = args or {}
end

function CallExpression:expressionNode()
	return
end

function CallExpression:TokenLiteral()
	return self.Token.Literal
end

function CallExpression:__tostring()
	local args = {}

	for _, arg in ipairs(self.Arguments) do
		table.insert(args, tostring(arg))
	end

	return string.format(
		"%s(%s)",
		tostring(self.Function),
		table.concat(args, ", ")
	)
end
CallExpression.__metatable = "CallExpression"

---@class Program
---@field Statements Statement[]
---@field TokenLiteral fun(self: Program): string
---@field new fun(): Program
Program = oo.class()
function Program:init()
	self.Statements = {}
end

function Program:TokenLiteral()
	if #self.Statements > 0 then
		return self.Statements[1]:TokenLiteral()
	else
		return ""
	end
end

function Program:__tostring()
	local result = {}
	for _, statement in ipairs(self.Statements) do
		table.insert(result, tostring(statement))
	end
	return table.concat(result, "")
end
Program.__metatable = "Program"

return {
	Program = Program,
	LetStatement = LetStatement,
	Expression = Expression,
	Identifier = Identifier,
	ReturnStatement = ReturnStatement,
	ExpressionStatement = ExpressionStatement,
	IntegerLiteral = IntegerLiteral,
	PrefixExpression = PrefixExpression,
	InfixExpression = InfixExpression,
	Boolean = Boolean
}