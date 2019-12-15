#!/bin/bash

brew install caskroom/cask/brew-cask

# must have
brew cask install \
  1password \
  alfred \
  choosy \
  dash \
  google-chrome \
  itsycal \
  kitty \
  notable \
  postbox \
  sublime-merge \
  telegram \
  tower \
  visual-studio-code \
  zoomus

# optional
brew cask install \
  acorn \
  aerial \
  bartender \
  bitbar \
  charles \
  chromedriver \
  contexts \
  daisydisk \
  figma \
  fork \
  grammarly \
  hyperdock \
  iina \
  imageoptim \
  monodraw \
  ngrok \
  numi \
  paw \
  postico \
  skype \
  spotify \
  textual \
  transmission

# fonts
brew tap caskroom/fonts
brew cask install font-mononoki font-cascadia font-iosevka

# quicklool plugins
brew cask install \
  epubquicklook \
  qlcolorcode \
  qlimagesize \
  qlmarkdown \
  qlprettypatch \
  qlstephen \
  qlvideo \
  quicklook-csv \
  quicklook-json \
  webpquicklook

brew cleanup -s
