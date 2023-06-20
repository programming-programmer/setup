require("mthich.remap")
require("mthich.setup")

-- THEME
local gruvbox = require("gruvbox")
vim.o.background = "dark"
gruvbox.setup({ transparent_mode = true, })
vim.cmd("colorscheme gruvbox")
