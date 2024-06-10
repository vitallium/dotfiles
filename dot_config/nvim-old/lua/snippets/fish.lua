local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node

return {
  s("if", {
    t("if "),
    i(0),
    t({ "", "end" }),
  }),
  s("elif", {
    t("else if "),
    i(0),
    t({ "", "" }),
  }),
  s("function", {
    t("function "),
    i(0),
    t({ "", "end" }),
  }),
  s("for", {
    t("for "),
    i(1),
    t(" in "),
    i(2),
    t({ "", "end" }),
  }),
}
