return {
  "echasnovski/mini.bufremove",
  init = function()
    vim.api.nvim_create_user_command("BDelete", function(args)
      require("mini.bufremove").delete(0, args.bang)
    end, { bang = true })

    vim.api.nvim_create_user_command("BWipeout", function(args)
      require("mini.bufremove").wipeout(0, args.bang)
    end, { bang = true })

    vim.keymap.set("n", "<leader>bd", function()
      require("mini.bufremove").delete()
    end, { desc = "Delete Buffer" })
  end,
  opts = {
    silent = true,
  },
}
