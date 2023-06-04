local builtin = require('telescope.builtin')
vim.g.mapleader = " "

vim.keymap.set("n", "<leader>pv", vim.cmd.Ex)
vim.keymap.set("i", "<C-g>", "<Esc>")

vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set("n", "n", "nzzzv")
vim.keymap.set("n", "N", "Nzzzv")

-- I'm going to put tab related stuff here for simplicity ¯\_(ツ)_/¯ 
vim.keymap.set('n', '<leader>tn', vim.cmd.tabnext)
vim.keymap.set('n', '<leader>tp', vim.cmd.tabprevious)
vim.keymap.set('n', '<leader>tt', vim.cmd.tabnew)
vim.keymap.set('n', '<leader>td', vim.cmd.tabclose)

-- Buffers via telescope
vim.keymap.set('n', '<leader>b', builtin.buffers,  {})
vim.keymap.set('n', '<leader>bd', vim.cmd.bdelete, {})
