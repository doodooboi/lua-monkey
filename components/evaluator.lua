---@diagnostic disable: lowercase-global
local object = require("components.object")
local builtin = require("components.builtin")

require("utility.utility")

local byReferenceEquality = {
	[object.types.BOOLEAN_OBJ] = true,
	[object.types.NULL_OBJ] = true,
}

---@param bool boolean
---@return Boolean
local function boolToObject(bool)
	if bool then
		return object.constants.TRUE
	else
		return object.constants.FALSE
	end
end

---@param obj BaseObject
---@return boolean
local function isError(obj)
	if obj then
		return obj:Type() == object.types.ERROR_OBJ
	end

	return false
end

---@param format string
---@param ... string
---@return Error
local function newError(format, ...)
	return object.Error.new(string.format(format, ...))
end

---@param node BaseObject
---@return boolean
local function truthy(node)
	if node == object.constants.FALSE or node == object.constants.NULL then
		return false
	end

	return true
end

---@param node Program
---@param env Environment
---@return BaseObject
local function evalProgram(node, env)
	local result = nil

	for _, stmt in ipairs(node.Statements) do
		result = eval(stmt, env)

		if result and result:Type() == object.types.RETURN_VALUE_OBJ then
			---@cast result ReturnValue

			return result.Value
		elseif result and result:Type() == object.types.ERROR_OBJ then
			return result
		end
	end

	return result
end

---@param right BaseObject
local function evalBangOperatorExpression(right)
	if right == object.constants.TRUE then
		return object.constants.FALSE
	elseif right == object.constants.FALSE or right == object.constants.NULL then
		return object.constants.TRUE
	end

	return object.constants.FALSE
end

---@param right BaseObject
---@return BaseObject
local function evalMinusPrefixExpression(right)
	if right:Type() ~= object.types.INTEGER_OBJ then
		return newError("unknown operator: -%s", right:Type())
	end

	---@cast right Integer
	return object.Integer.new(-right.Value)
end

---@param operator string
---@param right BaseObject
---@return BaseObject
local function evalPrefixExpression(operator, right)
	if operator == "!" then
		return evalBangOperatorExpression(right)
	elseif operator == '-' then
		return evalMinusPrefixExpression(right)
	end

	return newError("unknown operator: %s%s", operator, right:Type())
end

---@param left BaseObject -- The left object being checked
---@param right BaseObject -- The right object being checked
---@param expectedType string? -- Optional type check
---@return boolean -- True if both have the same type as each other
local function sameType(left, right, expectedType)
	local same = left:Type() == right:Type()
	if not same then return false end

	if expectedType then
		return left:Type() == expectedType
	end

	return true
end

---@param operator string
---@param left Integer
---@param right Integer
---@return BaseObject
local function evalIntegerInfixExpression(operator, left, right)
	if operator == "+" then
		return object.Integer.new(left.Value + right.Value)
	elseif operator == "-" then
		return object.Integer.new(left.Value - right.Value)
	elseif operator == "*" then
		return object.Integer.new(left.Value * right.Value)
	elseif operator == "/" then
		return object.Integer.new(left.Value / right.Value)
	end

	if operator == "<" then
		return boolToObject(left.Value < right.Value)
	elseif operator == ">" then
		return boolToObject(left.Value > right.Value)
	elseif operator == "==" then
		return boolToObject(left.Value == right.Value)
	elseif operator == "!=" then
		return boolToObject(left.Value ~= right.Value)
	end

	return newError("unknown operator: %s %s %s", left:Type(), operator, right:Type())
end

---@param operator string
---@param left String
---@param right String
---@return BaseObject
local function evalStringInfixExpression(operator, left, right)
	if operator == "+" then
		-- actual concatenation is slow, and tables are faster
		local str = { left.Value, right.Value }

		return object.String.new(table.concat(str))
	elseif operator == "==" then
		return boolToObject(left.Value == right.Value)
	elseif operator == "!=" then
		return boolToObject(left.Value ~= right.Value)
	end

	return newError(
		"unknown operator: %s %s %s",
		left:Type(),
		operator,
		right:Type()
	)
end

---@param operator string
---@param left BaseObject
---@param right BaseObject
---@return BaseObject
local function evalInfixExpression(operator, left, right)
	if sameType(left, right, object.types.INTEGER_OBJ) then
		---@cast left Integer
		---@cast right Integer

		return evalIntegerInfixExpression(operator, left, right)
	end

	if not sameType(left, right) then
		return newError("type mismatch: %s %s %s", left:Type(), operator, right:Type())
	end

	if left:Type() == object.types.STRING_OBJ then
		return evalStringInfixExpression(operator, left, right)
	end

	-- print(operator, left, typeof(left), right, typeof(right))
	if operator == "==" then
		if byReferenceEquality[left:Type()] then
			return boolToObject(left == right)
		else
			-- TODO: not do this
			return boolToObject(true)
		end
	elseif operator == "!=" then
		return boolToObject(left ~= right)
	end

	return newError("unknown operator: %s %s %s", left:Type(), operator, right:Type())
end

---@param node IfExpression
---@param env Environment
---@return BaseObject
local function evalIfExpression(node, env)
	local condition = eval(node.Condition, env)
	if isError(condition) then
		return condition
	end

	if truthy(condition) then
		return eval(node.Consequence, env)
	elseif node.Alternative then
		return eval(node.Alternative, env)
	end

	return object.constants.NULL
end

---@param node BlockStatement
---@param env Environment
---@return BaseObject
local function evalBlockStatement(node, env)
	local result = nil

	for _, statement in ipairs(node.Statements) do
		result = eval(statement, env)

		if result and (result:Type() == object.types.RETURN_VALUE_OBJ or result:Type() == object.types.ERROR_OBJ) then
			return result
		end
	end

	return result
end

---@param node Identifier
---@param env Environment
local function evalIdentifier(node, env)
	local value, ok = env:get(node.Value)
	if ok then return value end

	return builtin[node.Value] or newError("identifier not found: %s", node.Value)
end

---@param exprs Expression[]
---@param env Environment
---@return BaseObject[]
local function evalExpressions(exprs, env)
	local result = {}

	for _, expr in ipairs(exprs) do
		local evaluated = eval(expr, env)

		if isError(evaluated) then
			return { evaluated }
		end

		table.insert(result, evaluated)
	end

	return result
end

---@param node BaseObject
---@return BaseObject
local function unwrapReturnValue(node)
	if node:Type() == object.types.RETURN_VALUE_OBJ then
		---@cast node ReturnValue

		return node.Value
	end

	return node
end

---@param fn Function
---@param args BaseObject[]
---@return Environment
local function extendFunctionEnv(fn, args)
	local env = object.Environment.new(fn.Env)

	for index, param in ipairs(fn.Parameters) do
		env:set(param.Value, args[index])
	end

	return env
end

---@param fn BaseObject
---@param args BaseObject[]
---@return BaseObject
local function applyFunction(fn, args)
	if fn:Type() ~= object.types.FUNCTION_OBJ and fn:Type() ~= object.types.BUILTIN_OBJ then
		return newError("not a function: %s", fn:Type())
	end

	if fn:Type() == object.types.BUILTIN_OBJ then
		---@cast fn Builtin
		return fn.Fn(args)
	end
	---@cast fn Function

	local extendedEnv = extendFunctionEnv(fn, args)
	local evaluated = eval(fn.Body, extendedEnv)

	return unwrapReturnValue(evaluated)
end

---@param arr Array
---@param index Integer
---@return boolean
local function arrayIndexInBounds(arr, index)
	local idx = index.Value
	local max = #arr.Elements - 1

	return not (idx < 0 or idx > max)
end

---@param left BaseObject
---@param index BaseObject
---@return BaseObject
local function evalIndexExpression(left, index)
	if left:Type() == object.types.ARRAY_OBJ and index:Type() == object.types.INTEGER_OBJ then
		---@cast left Array
		---@cast index Integer

		return left.Elements[index.Value + 1] or object.constants.NULL
	end

	return newError("index operator not supported: %s", left:Type())
end

---@param arr BaseObject
---@param index BaseObject
---@param value BaseObject
---@return BaseObject
local function evalIndexAssignmentExpression(arr, index, value)
	if
		arr:Type() == object.types.ARRAY_OBJ and
		index:Type() == object.types.INTEGER_OBJ
	then
		---@cast arr Array
		---@cast index Integer

		arr.Elements[index.Value + 1] = value
		return
	end

	return newError("index operator not supported: %s", index:Type())
end

---@param node Node
---@param env Environment
function eval(node, env)
	local type = typeof(node)

	-- Statements
	if type == "Program" then
		---@cast node Program
		return evalProgram(node, env)
	elseif type == "ExpressionStatement" then
		---@cast node ExpressionStatement
		return eval(node.Expression, env)
	elseif type == "BlockStatement" then
		---@cast node BlockStatement

		return evalBlockStatement(node, env)
	elseif type == "ReturnStatement" then
		---@cast node ReturnStatement
		local value = eval(node.ReturnValue, env)

		if isError(value) then
			return value
		end

		return object.ReturnValue.new(value)
	elseif type == "LetStatement" then
		---@cast node LetStatement
		local value = eval(node.Value, env)
		if isError(value) then
			return value
		end

		if builtin[node.Name.Value] then
			return newError("cannot modify constant")
		end

		env:set(node.Name.Value, value)
	elseif type == "AssignmentStatement" then
		---@cast node AssignmentStatement
		local ident = evalIdentifier(node.Name, env)
		if isError(ident) then
			return ident
		end

		local value = eval(node.Value, env)
		if isError(value) then
			return value
		end

		if builtin[node.Name.Value] then
			return newError("cannot modify constant")
		end

		env:set(node.Name.Value, value)
	elseif type == "ArrayLiteral" then
		---@cast node ArrayLiteral
		local elements = evalExpressions(node.Elements, env)
		if #elements == 1 and isError(elements[1]) then
			return elements[1]
		end

		return object.Array.new(elements)
	end

	if type == "Identifier" then
		---@cast node Identifier
		return evalIdentifier(node, env)
	elseif type == "FunctionLiteral" then
		---@cast node FunctionLiteral

		return object.Function.new(node.Body, node.Parameters, env)
	end

	-- Expressions
	if type == "IntegerLiteral" then
		---@cast node IntegerLiteral
		return object.Integer.new(node.Value)
	elseif type == "BooleanLiteral" then
		---@cast node BooleanLiteral

		return boolToObject(node.Value)
	elseif type == "StringLiteral" then
		---@cast node StringLiteral

		return object.String.new(node.Value)
	elseif type == "PrefixExpression" then
		---@cast node PrefixExpression
		local right = eval(node.Right, env)

		if isError(right) then
			return right
		end

		return evalPrefixExpression(node.Operator, right)
	elseif type == "InfixExpression" then
		---@cast node InfixExpression

		local left = eval(node.Left, env)
		if isError(left) then
			return left
		end

		local right = eval(node.Right, env)
		if isError(right) then
			return right
		end

		return evalInfixExpression(node.Operator, left, right)
	elseif type == "IndexExpression" then
		---@cast node IndexExpression
		local left = eval(node.Left, env)
		if isError(left) then
			return left
		end

		local index = eval(node.Index, env)
		if isError(index) then
			return index
		end

		return evalIndexExpression(left, index)
	elseif type == "ArrayIndexExpression" then
		---@cast node ArrayIndexExpression
		local array = eval(node.Array, env)
		if isError(array) then
			return array
		end

		local index = eval(node.Index, env)
		if isError(index) then
			return index
		end

		local value = eval(node.Value, env)
		if isError(value) then
			return value
		end

		evalIndexAssignmentExpression(array, index, value)
		return
	elseif type == "IfExpression" then
		---@cast node IfExpression

		return evalIfExpression(node, env)
	elseif type == "CallExpression" then
		---@cast node CallExpression

		local fun = eval(node.Function, env)
		---@cast fun Function

		if isError(fun) then
			return fun
		end

		local args = evalExpressions(node.Arguments, env)
		if #args == 1 and isError(args[1]) then
			return args[1]
		end

		return applyFunction(fun, args)
	end
end
