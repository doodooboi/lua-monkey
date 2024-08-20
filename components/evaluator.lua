---@diagnostic disable: lowercase-global
local object = require("components.object")

require("utility.utility")

local constants = {
    TRUE = object.Boolean.new(true),
    FALSE = object.Boolean.new(false),
    NULL = object.Null.new()
}

---@param bool boolean
---@return Boolean
local function boolToObject(bool)
    if bool then
        return constants.TRUE
    else
        return constants.FALSE
    end
end

---@param node BaseObject
---@return boolean
local function truthy(node)
    if node == constants.FALSE or node == constants.NULL then
        return false
    end

    return true
end

---@param node Program
---@return BaseObject
local function evalProgram(node)
    local result = nil

    for _, stmt in ipairs(node.Statements) do
        result = eval(stmt)

        if result and result:Type() == object.types.RETURN_VALUE_OBJ then
            ---@cast result ReturnValue
            return result.Value
        end
    end

    return result
end

---@param right BaseObject
local function evalBangOperatorExpression(right)
    if right == constants.TRUE then
        return constants.FALSE
    elseif right == constants.FALSE or right == constants.NULL then
        return constants.TRUE
    end

    return constants.FALSE
end

---@param right BaseObject
---@return BaseObject
local function evalMinusPrefixExpression(right)
    if right:Type() ~= object.types.INTEGER_OBJ then
        return constants.NULL
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

    return constants.NULL
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

    return constants.NULL
end


---@param operator string
---@param left BaseObject
---@param right BaseObject
---@return BaseObject
local function evalInfixExpression(operator, left, right)
    if left:Type() == object.types.INTEGER_OBJ and right:Type() == object.types.INTEGER_OBJ then
        ---@cast left Integer
        ---@cast right Integer
  
        return evalIntegerInfixExpression(operator, left, right)
    end

    if operator == "==" then
        return boolToObject(left == right)
    elseif operator == "!=" then
        return boolToObject(left ~= right)
    end

    return constants.NULL
end

---@param node IfExpression
---@return BaseObject
local function evalIfExpression(node)
    local condition = eval(node.Condition)

    if truthy(condition) then
        return eval(node.Consequence)
    elseif node.Alternative then
        return eval(node.Alternative)
    end

    return constants.NULL
end

---@param node BlockStatement
---@return BaseObject
local function evalBlockStatement(node)
    local result = nil

    for _, statement in ipairs(node.Statements) do
        result = eval(statement)

        if result and result:Type() == object.types.RETURN_VALUE_OBJ then
            return result
        end
    end

    return result
end

---@param node Node
function eval(node)
    local type = typeof(node)

    -- Statements
    if type == "Program" then
        ---@cast node Program
        return evalProgram(node)
    elseif type == "ExpressionStatement" then
        ---@cast node ExpressionStatement
        return eval(node.Expression)
    elseif type == "BlockStatement" then
        ---@cast node BlockStatement
        
        return evalBlockStatement(node)
    elseif type == "ReturnStatement" then
        ---@cast node ReturnStatement
        
        return object.ReturnValue.new(eval(node.ReturnValue))
    end

    -- Expressions
    if type == "IntegerLiteral" then
        ---@cast node IntegerLiteral
        return object.Integer.new(node.Value)
    elseif type == "BooleanLiteral" then
        ---@cast node BooleanLiteral
        
        return boolToObject(node.Value)
    elseif type == "PrefixExpression" then
        ---@cast node PrefixExpression
        local right = eval(node.Right)

        return evalPrefixExpression(node.Operator, right)
    elseif type == "InfixExpression" then
        ---@cast node InfixExpression
        
        local left = eval(node.Left)
        local right = eval(node.Right)

        return evalInfixExpression(node.Operator, left, right)
    elseif type == "IfExpression" then
        ---@cast node IfExpression
        
        return evalIfExpression(node)
    end
end


