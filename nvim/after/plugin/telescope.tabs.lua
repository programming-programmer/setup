local tabs = require('telescope-tabs')

vim.keymap.set('n', '<leader>tl', tabs.list_tabs, {})

-- I'm going to put tab related stuff here for simplicity ¯\_(ツ)_/¯ 
vim.keymap.set('n', '<leader>tn', vim.cmd.tabnext)
vim.keymap.set('n', '<leader>tp', vim.cmd.tabprevious)
vim.keymap.set('n', '<leader>tt', vim.cmd.tabnew)
vim.keymap.set('n', '<leader>td', vim.cmd.tabclose)




