vim.opt.cursorline = true

vim.opt.laststatus = 0
vim.opt.ruler = false
vim.opt.showcmd = false

vim.opt.nu = true
vim.opt.relativenumber = true

vim.opt.scrolloff = 10
vim.opt.signcolumn = "yes"
vim.opt.isfname:append("@-@")

vim.opt.hlsearch = false
vim.opt.incsearch = true

vim.opt.wrap = false

vim.opt.smartindent = true

vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true

vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = os.getenv("HOME") .. "/.cache/nvim/undodir"
vim.opt.undofile = true

vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1
