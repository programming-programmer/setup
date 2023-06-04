local builtin = require('telescope.builtin')
vim.g.mapleader = " "

vim.keymap.set("n", "<leader>pv", vim.cmd.Ex)
vim.keymap.set("i", "<C-g>", "<Esc>")

-- Sweet mother of magaldene, this brings a tear to me eye
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

-- NO disoreinting movement
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set("n", "n", "nzzzv")
vim.keymap.set("n", "N", "Nzzzv")

-- System clipboard pasting (I will cry)
-- next greatest remap ever : asbjornHaland
vim.keymap.set({"n", "v"}, "<leader>y", [["+y]])
vim.keymap.set("n", "<leader>Y", [["+Y]])

-- Beugtilf sdfkaldf replace WOOWWOWOW!!
vim.keymap.set("n", "<leader>s", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]])

-- I'm going to put tab related stuff here for simplicity ¯\_(ツ)_/¯ 
vim.keymap.set('n', '<leader>tt', vim.cmd.tabnew)
vim.keymap.set('n', '<leader>td', vim.cmd.tabclose)

-- Buffers via telescope
vim.keymap.set('n', '<leader>bb', builtin.buffers,  {})
vim.keymap.set('n', '<leader>bd', vim.cmd.bdelete, {})
