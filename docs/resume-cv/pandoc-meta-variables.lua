local vars = {}

local function get_vars(meta)
	for k, v in pairs(meta) do
		if pandoc.utils.type(v) == 'Inlines' then
			vars["%" .. k .. "%"] = {table.unpack(v)}
		end
	end
end

local function replace(el)
	if vars[el.text] then
		return vars[el.text]
	else
		return el
	end
end

return {{Meta = get_vars}, {Str = replace}}
