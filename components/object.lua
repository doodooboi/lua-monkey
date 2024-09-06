-- Page 110 of the PDF

local oo = require("utility.oo")

---@enum types
local types = {
	INTEGER_OBJ = "INTEGER",
	BOOLEAN_OBJ = "BOOLEAN",
	NULL_OBJ = "NULL",
	FUNCTION_OBJ = "FUNCTION",
	BUILTIN_OBJ = "BUILTIN",
	STRING_OBJ = "STRING",

	RETURN_VALUE_OBJ = "RETURN",
	ERROR_OBJ = "ERROR",

	ARRAY_OBJ = "ARRAY"
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

---@class String: BaseObject
---@field Value string
---@field new fun(value: string): String
local String = oo.class(BaseObject)

function String:init(value)
	BaseObject.init(self)

	self.Value = value
end

function String:Inspect()
	return self.Value
end

function String:Type()
	return types.STRING_OBJ
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

---@alias BuiltinFunction fun(...: BaseObject): BaseObject

---@class Builtin: BaseObject
---@field Fn BuiltinFunction
---@field new fun(fn: BuiltinFunction): Builtin
local Builtin = oo.class(BaseObject)

function Builtin:init(fn)
	BaseObject.init(self)

	self.Fn = fn
end

function Builtin:Inspect()
	return "builtin function"
end

function Builtin:Type()
	return types.BUILTIN_OBJ
end

---@class Environment
---@field store {[string]: {value: BaseObject, const: boolean}}
---@field outer Environment?
---@field new fun(outer: Environment?): Environment
---@field get fun(self: Environment, name: string): ({value: BaseObject, const: boolean}, boolean)
---@field set fun(self: Environment, name: string, value: BaseObject, const: boolean): BaseObject
-- really wanted a generic up there, but wont work :c
local Environment = oo.class()

function Environment:init(outer)
	self.store = {}
	self.outer = outer
end

function Environment:get(name)
	local got = self.store[name]

	if got == nil and self.outer then
		got = self.outer:get(name)
	end

	return got, got ~= nil
end

function Environment:set(name, value, const)
	self.store[name] = {value = value, const = const}

	return value
end

---@class Function: BaseObject
---@field Body BlockStatement
---@field Parameters Identifier[]
---@field Env Environment
---@field new fun(body: BlockStatement, params: Identifier[], env: Environment): Function
local Function = oo.class(BaseObject)

function Function:init(body, params, env)
	BaseObject.init(self)

	self.Body = body
	self.Parameters = params or {}
	self.Env = env
end

function Function:Inspect()
	local params = {}

	for _, param in ipairs(self.Parameters) do
		table.insert(params, tostring(param))
	end

	return string.format(
		"fn(%s) {\n%s\n}",
		table.concat(params, ", "),
		tostring(self.Body)
	)
end

function Function:Type()
	return types.FUNCTION_OBJ
end

---@class Array: BaseObject
---@field Elements BaseObject[]
---@field new fun(elements: BaseObject[]): Array
local Array = oo.class(BaseObject)

function Array:init(elements)
	BaseObject.init(self)

	self.Elements = elements
end

function Array:Type()
	return types.ARRAY_OBJ
end

function Array:Inspect()
	local elements = {}

	for _, elmnt in ipairs(self.Elements) do
		table.insert(elements, elmnt:Inspect())
	end

	return string.format(
		"[%s]",
		table.concat(elements, ", ")
	)
end

---@class Hash: BaseObject
---@field new fun()

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

local constants = {
	TRUE = Boolean.new(true),
	FALSE = Boolean.new(false),
	NULL = Null.new()
}

return {
	ReturnValue = ReturnValue,
	Integer = Integer,
	Boolean = Boolean,
	String = String,
	Error = Error,
	Null = Null,

	Function = Function,
	Builtin = Builtin,

	Environment = Environment,
	Array = Array,

	types = types,
	constants = constants,
}
