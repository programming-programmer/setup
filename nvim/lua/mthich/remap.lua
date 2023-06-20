-- WHERE"S THE MAP!@@!@#>>? HERE IT IS!!!!
vim.g.mapleader = " "

-- Yall got me FUNKED up if you think I'm reachin my teenie pinkie for that teeny escape key
vim.keymap.set("i", "<C-g>", "<Esc>")

-- Sweet mother of magaldene, this brings a tear to my eye (also a pain in my back)
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

-- NO deisiointing movemnet
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set("n", "n", "nzzzv")
vim.keymap.set("n", "N", "Nzzzv")

-- CLIP DAT THING MRS> DJFKSFLJDSYAY@@
vim.keymap.set({"n", "v"}, "<leader>y", [["+y]])
vim.keymap.set("n", "<leader>Y", [["+Y]])

-- WOOW REPLACE AANNNNDD EXEC. it must be heaven :)
vim.keymap.set("n", "<leader>x", "<cmd>!chmod +x %<CR>", { silent = true })
vim.keymap.set("n", "<leader>s", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]])

-- Tab and buffer creation/deletion
vim.keymap.set("n", "<leader>tt", vim.cmd.tabnew)
vim.keymap.set("n", "<leader>td", vim.cmd.tabclose)
vim.keymap.set("n", "<leader>bb", vim.cmd.buffers)
vim.keymap.set("n", "<leader>bd", vim.cmd.bdelete)

-- Easily source file
vim.keymap.set("n", "<leader><leader>", function()
	vim.cmd("so")
end)
