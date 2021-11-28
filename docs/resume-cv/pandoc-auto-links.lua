local links = {}

function get_auto_links(meta)
	if (type(meta.auto_links) == 'table' or type(meta.auto_links) == 'userdata') and meta.auto_links.t == 'MetaMap' then
		for k, v in pairs(meta.auto_links) do
			if (type(v) == 'table' or type(v) == 'userdata') and v.t == 'MetaInlines' then
				t = table.unpack(v)
				if (type(t) == 'table' or type(t) == 'userdata') and t.t == 'Str' then
					links[k] = t.text
				end
			end
		end
	end
end

function replace(el)
	-- TODO: comma after text
	if links[el.text] then
		return pandoc.Link(el, links[el.text])
	else
		return el
	end
end

return {{Meta = get_auto_links}, {Str = replace}}
