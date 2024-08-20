-- Page 110 of the PDF

local oo = require("utility.oo")

---@enum types
local types = {
    INTEGER_OBJ = "INTEGER",
    BOOLEAN_OBJ = "BOOLEAN",
    NULL_OBJ = "NULL",

    RETURN_VALUE_OBJ = "RETURN",
    ERROR_OBJ = "ERROR"
}

---@class BaseObject
---@field Type fun(self: BaseObject): string
---@field Inspect fun(self: BaseObject): string
local BaseObject = oo.class()
function BaseObject:init() end

function BaseObject:Inspect()
    return "BaseObject::Inspect must be overriden"
end

function BaseObject:Type()
    return "BaseObject::Type must be overriden"
end

---@class Integer: BaseObject
---@field Value number
---@field new fun(value: number): Integer
local Integer = oo.class(BaseObject)

function Integer:init(value)
    BaseObject.init(self)

    self.Value = value
end

function Integer:Inspect()
    return tostring(self.Value)
end

function Integer:Type()
    return types.INTEGER_OBJ
end

---@class Boolean: BaseObject
---@field Value boolean
---@field new fun(value: boolean): Boolean
local Boolean = oo.class(BaseObject)

function Boolean:init(value)
    BaseObject.init(self)

    self.Value = value
end

function Boolean:Inspect()
    return tostring(self.Value)
end

function Boolean:Type()
    return types.BOOLEAN_OBJ
end

---@class ReturnValue: BaseObject
---@field Value BaseObject
---@field new fun(value: BaseObject): ReturnValue
local ReturnValue = oo.class(BaseObject)

function ReturnValue:init(value)
    BaseObject.init(self)

    self.Value = value
end

function ReturnValue:Inspect()
    return self.Value:Inspect()
end

function ReturnValue:Type()
    return types.RETURN_VALUE_OBJ
end

---@class Error: BaseObject
---@field Message string
---@field new fun(msg: string): Error
local Error = oo.class(BaseObject)

function Error:init(msg)
    BaseObject.init(self)

    self.Message = msg
end

function Error:Inspect()
    return string.format("Error: %s", self.Message)
end

function Error:Type()
    return types.ERROR_OBJ
end

---@class Environment
---@field store {[string]: BaseObject}
---@field new fun(): Environment
---@field get fun(self: Environment, name: string): (BaseObject, boolean)
---@field set fun(self: Environment, name: string, value: BaseObject): BaseObject
-- really wanted a generic up there, but wont work :c
local Environment = oo.class()

function Environment:init()
    self.store = {}
end

function Environment:get(name) 
    local got = self.store[name]

    return got, got ~= nil
end

function Environment:set(name, value)
    self.store[name] = value

    return value
end

---@class Null: BaseObject
---@field new fun(): Null
local Null = oo.class(BaseObject)

function Null:init()
    BaseObject.init(self)
end

function Null:Inspect()
    return "null"
end

function Null:Type()
    return types.NULL_OBJ
end

return {
    Integer = Integer,
    Boolean = Boolean,
    Null = Null,
    ReturnValue = ReturnValue,
    Error = Error,

    Environment = Environment,

    types = types
}