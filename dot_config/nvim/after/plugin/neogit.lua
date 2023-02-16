local status_ok_neogit, neogit = pcall(require, 'neogit')
if not status_ok_neogit then
  return
end

neogit.setup {
  disable_builtin_notifications = true
}
