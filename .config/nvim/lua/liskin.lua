local M = {}

function M.src(path)
	vim.cmd.source(vim.fn.stdpath("config") .. "/" .. path)
end

return M
