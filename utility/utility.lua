---@diagnostic disable: lowercase-global

---@param obj any
---@return string
function typeof(obj)
	local mt = getmetatable(obj)

	if (mt and type(mt) == "string") then
		return mt
	end

	if obj and obj.Type and type(obj.Type) == "function" then
		return obj:Type()
	end

	return type(obj)
end

function serializeTable(val, name, skipnewlines, depth)
	skipnewlines = skipnewlines or false
	depth = depth or 0

	local tmp = string.rep(" ", depth)

	if name then tmp = tmp .. name .. " = " end

	if type(val) == "table" then
		tmp = tmp .. "{" .. (not skipnewlines and "\n" or "")

		for k, v in pairs(val) do
			tmp = tmp .. serializeTable(v, k, skipnewlines, depth + 1) .. "," .. (not skipnewlines and "\n" or "")
		end

		tmp = tmp .. string.rep(" ", depth) .. "}"
	elseif type(val) == "number" then
		tmp = tmp .. tostring(val)
	elseif type(val) == "string" then
		tmp = tmp .. string.format("%q", val)
	elseif type(val) == "boolean" then
		tmp = tmp .. (val and "true" or "false")
	else
		tmp = tmp .. "\"[inserializeable datatype:" .. type(val) .. "]\""
	end

	return tmp
end

function len(tbl)
	local size = 0

	for _ in pairs(tbl) do
		size = size + 1
	end

	return size
end