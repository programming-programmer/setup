vim.cmd.packadd('packer.nvim')

return require('packer').startup(function(use)
	-- Look at packer, managing itself and whatnot
	use('wbthomason/packer.nvim')


  -- TREESITTER --
  use {
      'nvim-treesitter/nvim-treesitter',
      run = function()
          local ts_update = require('nvim-treesitter.install').update({ with_sync = true })
          ts_update()
      end,
  }

  use('nvim-treesitter/playground')

  -- FILE NAV --
  use {
      'nvim-telescope/telescope.nvim', tag = '0.1.1',
      -- or                            , branch = '0.1.x',
      requires = { {'nvim-lua/plenary.nvim'} }
  }

  use('ThePrimeagen/harpoon')
  use('stevearc/oil.nvim')

  -- DEV --
  use('mbbill/undotree')
  use('tpope/vim-fugitive')
  use('lukas-reineke/indent-blankline.nvim')

  use {
      'folke/trouble.nvim',
      config = function()
          require('trouble').setup {
              icons = false,
          }
      end
  }

  -- LSP Setup --
  use {
      'VonHeikemen/lsp-zero.nvim',
      branch = 'v1.x',
      requires = {
          -- LSP Support
          {'neovim/nvim-lspconfig'},
          {'williamboman/mason.nvim'},
          {'williamboman/mason-lspconfig.nvim'},

          -- Autocompletion
          {'hrsh7th/nvim-cmp'},
          {'hrsh7th/cmp-buffer'},
          {'hrsh7th/cmp-path'},
          {'saadparwaiz1/cmp_luasnip'},
          {'hrsh7th/cmp-nvim-lsp'},
          {'hrsh7th/cmp-nvim-lua'},

          -- Snippets
          {'L3MON4D3/LuaSnip'},
          {'rafamadriz/friendly-snippets'},
      }
  }

  -- Debugging
  use {
      'rcarriga/nvim-dap-ui',
      requires = {
          {'mfussenegger/nvim-dap'}
      }
  }

  use('theHamsta/nvim-dap-virtual-text')

  -- Java
  use('mfussenegger/nvim-jdtls')

  -- AESTHETICS --
  use("ellisonleao/gruvbox.nvim")

  use {
      'goolord/alpha-nvim',
      config = function ()
          require'alpha'.setup(require'alpha.themes.dashboard'.config)
      end
  }

  -- MISC --
  use('ThePrimeagen/vim-be-good')
end)
