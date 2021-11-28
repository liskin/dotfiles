local vars = {}

function get_vars(meta)
	for k, v in pairs(meta) do
		if (type(v) == 'table' or type(v) == 'userdata') and v.t == 'MetaInlines' then
			vars["%" .. k .. "%"] = {table.unpack(v)}
		end
	end
end

function replace(el)
	if vars[el.text] then
		return vars[el.text]
	else
		return el
	end
end

return {{Meta = get_vars}, {Str = replace}}
