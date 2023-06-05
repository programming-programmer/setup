require("mthich15.remap")
require("mthich15.setup")

-- THEME: GRUVBOX
local gruvbox = require("gruvbox")
vim.o.background = "dark" -- or -"light" for light mode
gruvbox.setup({ transparent_mode = true, })
vim.cmd("colorscheme gruvbox")


