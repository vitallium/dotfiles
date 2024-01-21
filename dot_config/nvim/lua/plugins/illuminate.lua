return {
    -- Highlight similar words (e.g. references with LSP)
    "RRethy/vim-illuminate",
    event = { "BufReadPost", "BufNewFile" },
    opts = {
        filetypes_denylist = {
            "NvimTree",
            "NeogitCommitMessage",
            "NeogitStatus",
        },
    },
    config = function(_, opts)
        require("illuminate").configure(opts)
    end,
}
