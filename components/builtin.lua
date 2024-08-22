local object = require("components.object")
require("utility.utility")

---@param format string
---@param ... string
---@return Error
local function newError(format, ...)
	return object.Error.new(string.format(format, ...))
end

local builtins = {
	len = object.Builtin.new(function(args)
		if #args ~= 1 then
			return newError("expected 1 argument, got %s", tostring(#args))
		end

		local argument = args[1]

		if argument:Type() == object.types.STRING_OBJ then
			---@cast argument String
			local len = utf8.len(argument.Value)

			return object.Integer.new(len or 0)
		end
		
		return newError("expected %s, got %s", object.types.STRING_OBJ, argument:Type())
	end)
}

return builtins