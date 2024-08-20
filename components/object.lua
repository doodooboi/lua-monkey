-- Page 110 of the PDF

local oo = require("utility.oo")

---@enum types
local types = {
    INTEGER_OBJ = "INTEGER",
    BOOLEAN_OBJ = "BOOLEAN",
    NULL_OBJ = "NULL",

    RETURN_VALUE_OBJ = "RETURN"
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

    types = types
}