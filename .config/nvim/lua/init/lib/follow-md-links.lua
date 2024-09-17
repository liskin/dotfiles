-- Origin: https://github.com/jghauser/follow-md-links.nvim/blob/caa85cad973c5e612662d04b55e07eefc88f1d6b/lua/follow-md-links.lua
-- SPDX-License-Identifier: GPL-3.0-or-later

local ts = require("vim.treesitter")
local ts_utils = require("nvim-treesitter.ts_utils")

local M = {}

local function get_reference_link_destination(link_label)
	local syntax_trees = vim.treesitter.get_parser():parse(true)
	local root = syntax_trees[1]:root()

	local link_defs = {}
	local query_link_defs = vim.treesitter.query.parse("markdown", [[
		(link_reference_definition
			(link_label) @link_label
			(link_destination) @link_destination)
	]])
	for _, match, _ in query_link_defs:iter_matches(root, 0, nil, nil, { all = true }) do
		local captures = {}
		for id, nodes in pairs(match) do
			local text = ts.get_node_text(nodes[1], 0)
			captures[query_link_defs.captures[id]] = text
		end
		table.insert(link_defs, captures)
	end

	local link = vim.iter(link_defs):find(function(link) return link.link_label == link_label end)
	if link then
		return link.link_destination
	end
end

local function ts_find_child(node, type)
	for _, child in pairs(ts_utils.get_named_children(node)) do
		if child:type() == type then
			return child
		end
	end
end

local function get_link_destination()
	local node_at_cursor = ts.get_node({ ignore_injections = false })
	local node = node_at_cursor
	while node do
		if node:type() == "link_destination" then
			return ts.get_node_text(node, 0)
		elseif node:type() == "inline_link" or node:type() == "link_reference_definition" then
			local node_link_destination = ts_find_child(node, "link_destination")
			return node_link_destination and ts.get_node_text(node_link_destination, 0)
		elseif node:type() == "uri_autolink" then
			local link = ts.get_node_text(node, 0)
			return link:match("^<(.*)>$")
		elseif node:type()  == "shortcut_link" or node:type() == "collapsed_reference_link" then
			local node_link_text = ts_find_child(node, "link_text")
			if node_link_text then
				local link_text = ts.get_node_text(node_link_text, 0)
				return get_reference_link_destination("[" .. link_text .. "]")
			end
		elseif node:type() == "full_reference_link" then
			local node_link_label = ts_find_child(node, "link_label")
			if node_link_label then
				local link_label = ts.get_node_text(node_link_label, 0)
				return get_reference_link_destination(link_label)
			end
		elseif node:type() == "image" then
			local node_link_destination = ts_find_child(node, "link_destination")
			if node_link_destination then
				return ts.get_node_text(node_link_destination, 0)
			end

			local node_link_label = ts_find_child(node, "link_label")
			if node_link_label then
				local link_label = ts.get_node_text(node_link_label, 0)
				return get_reference_link_destination(link_label)
			end

			local node_image_description = ts_find_child(node, "image_description")
			if node_image_description then
				local image_description = ts.get_node_text(node_image_description, 0)
				return get_reference_link_destination("[" .. image_description .. "]")
			end
		end

		node = node:parent()
	end
end

local function resolve_link(link)
	local link_type
	if link:sub(1, 1) == [[/]] then
		link_type = "local"
		return link, link_type
	elseif link:sub(1, 1) == [[~]] then
		link_type = "local"
		return os.getenv("HOME") .. [[/]] .. link:sub(2), link_type
	elseif link:sub(1, 8) == [[https://]] or link:sub(1, 7) == [[http://]] then
		link_type = "web"
		return link, link_type
	else
		link_type = "local"
		return vim.fn.expand("%:p:h") .. [[/]] .. link, link_type
	end
end

function M.resolve_link_destination()
	local link_destination = get_link_destination()
	if link_destination then
		return resolve_link(link_destination)
	end
end

return M
