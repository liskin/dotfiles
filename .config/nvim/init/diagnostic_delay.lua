-- Pause all diagnostic updates in 'modified' buffers.
--
-- This mimics the behaviour of ALE with g:ale_lint_on_save=1,
-- g:ale_lint_on_insert_leave=0, g:ale_lint_on_text_changed=0.
--
-- Additionally, it happens to fix a bug of nvim's diagnostic implementation of
-- update_in_insert=false which hides all diagnostics in Insert mode (it delays
-- showing them but hides them immediately on update).

-- Metatable that automatically creates an empty table when assigning to a missing key
local bufnr_and_namespace_cacher_mt = {
	__index = function(t, bufnr)
		assert(bufnr > 0, 'Invalid buffer number')
		t[bufnr] = {}
		return t[bufnr]
	end,
}

local diag_delay_locked = {}
local diag_delay_queue = setmetatable({}, bufnr_and_namespace_cacher_mt)

local function get_bufnr(bufnr)
	if not bufnr or bufnr == 0 then
		return vim.api.nvim_get_current_buf()
	else
		return bufnr
	end
end

local orig_vim_diagnostic_set = vim.diagnostic.set
---@diagnostic disable-next-line: duplicate-set-field
function vim.diagnostic.set(namespace, bufnr, ...)
	vim.validate({
		namespace = { namespace, 'n' },
		bufnr = { bufnr, 'n' },
	})

	bufnr = get_bufnr(bufnr)

	if diag_delay_locked[bufnr] then
		diag_delay_queue[bufnr][namespace] = { ... }
	else
		orig_vim_diagnostic_set(namespace, bufnr, ...)
	end
end

local orig_vim_diagnostic_reset = vim.diagnostic.reset
---@diagnostic disable-next-line: duplicate-set-field
function vim.diagnostic.reset(namespace, bufnr)
	vim.validate({
		namespace = { namespace, 'n', true },
		bufnr = { bufnr, 'n', true },
	})

	-- Remove all relevant entries from the queue so we don't replay them later
	-- and then vim.diagnostic.reset

	local buffers = bufnr and { get_bufnr(bufnr) } or vim.tbl_keys(diag_delay_queue)
	for _, iter_bufnr in ipairs(buffers) do
		local namespaces = namespace and { namespace } or vim.tbl_keys(diag_delay_queue[iter_bufnr])
		for _, iter_namespace in ipairs(namespaces) do
			diag_delay_queue[iter_bufnr][iter_namespace] = nil
		end
	end

	orig_vim_diagnostic_reset(namespace, bufnr)
end

local function diag_delay_lock(bufnr)
	diag_delay_locked[bufnr] = true
end

local function diag_delay_unlock(bufnr)
	diag_delay_locked[bufnr] = false

	local queue = diag_delay_queue[bufnr]
	diag_delay_queue[bufnr] = nil

	for namespace, args in pairs(queue) do
		orig_vim_diagnostic_set(namespace, bufnr, unpack(args))
	end
end

local group_diag_delay = vim.api.nvim_create_augroup('LiskinDiagDelay', { clear = true })

vim.api.nvim_create_autocmd('BufWipeout', {
	group = group_diag_delay,
	callback = function(info)
		local bufnr = info.buf
		assert(bufnr > 0, 'Invalid buffer number')
		diag_delay_locked[bufnr] = nil
		diag_delay_queue[bufnr] = nil
	end,
})

vim.api.nvim_create_autocmd('BufModifiedSet', {
	group = group_diag_delay,
	callback = function(info)
		local bufnr = info.buf
		assert(bufnr > 0, 'Invalid buffer number')
		if vim.bo[bufnr].modified then
			diag_delay_lock(bufnr)
		else
			diag_delay_unlock(bufnr)
		end
	end,
})
