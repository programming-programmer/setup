local dap = require("dap")

vim.keymap.set('n', '<leader>b', function () dap.toggle_breakpoint() end)
vim.keymap.set('n', '<leader>B', function () dap.set_breakpoint(vim.fn.input('Breakpoint condition: ')) end)
vim.keymap.set('n', '<leader>lp', function () dap.set_breakpoint(nil, nil, vim.fn.input('Log point message: ')) end)
