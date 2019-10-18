#!/bin/bash

brew install caskroom/cask/brew-cask

# must have
brew cask install 1password
brew cask install alfred
brew cask install forklift
brew cask install google-chrome
brew cask install iterm2

# optional
brew cask install acorn
  \ aerial
  \ bartender
  \ charles
  \ chromedriver
  \ contexts
  \ daisydisk
  \ dash
  \ firefox
  \ fork
  \ grammarly
  \ hyperdock
  \ iina
  \ imageoptim
  \ macdown
  \ monodraw
  \ ngrok
  \ numi
  \ paw
  \ postico
  \ skype
  \ spotify
  \ textual
  \ transmission
  \ visual-studio-code

# fonts

brew tap caskroom/fonts
brew cask install font-mononoki font-source-code-pro font-cascadia

brew cleanup -s
