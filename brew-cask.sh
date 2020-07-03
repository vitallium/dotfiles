#!/bin/bash

brew install caskroom/cask/brew-cask

brew cask install \
  1password \
  acorn \
  aerial \
  alacritty \
  alfred \
  amethyst \
  bartender \
  charles \
  choosy \
  chromedriver \
  contexts \
  daisydisk \
  dash \
  docker \
  figma \
  firefox \
  fork \
  forklift \
  google-chrome \
  google-cloud-sdk \
  grammarly \
  hyperdock \
  iina \
  imageoptim \
  itsycal \
  karabiner-elements \
  keka \
  kitty \
  monitorcontrol \
  monodraw \
  ngrok \
  notable \
  numi \
  paw \
  postbox \
  postico \
  qmoji \
  skype \
  spotify \
  sublime-merge \
  telegram \
  textual \
  tower \
  transmission \
  visual-studio-code \
  zoomus \

# fonts
brew tap caskroom/fonts
brew cask install font-mononoki font-iosevka

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
