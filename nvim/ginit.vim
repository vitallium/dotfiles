set termguicolors
colorscheme gruvbox

if exists('g:GuiLoaded')
  GuiFont Mononoki Nerd Font
  GuiTabline 0
  GuiPopupmenu 0
  GuiLinespace 1

  " Mouse settings, mostly so copy-pasting in :term buffers is a bit easier
  set mouse=a
  set mousemodel=popup

  " Hack to work around https://github.com/equalsraf/neovim-qt/issues/259
  tnoremap <S-Backspace> <Backspace>
  tnoremap <S-Space> <Space>
  tnoremap <C-Backspace> <Backspace>
  tnoremap <C-Space> <Space>
  tnoremap <C-Enter> <Enter>
endif
