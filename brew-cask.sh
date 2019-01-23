#!/bin/bash

brew install caskroom/cask/brew-cask

# must have
brew cask install 1password
brew cask install alfred
brew cask install forklift
brew cask install google-chrome
brew cask install iterm2
brew cask install tower

# optional
brew cask install acorn
brew cask install aerial
brew cask install bartender
brew cask install charles
brew cask install daisydisk
brew cask install dash
brew cask install firefox
brew cask install fork
brew cask install grammarly
brew cask install hyperdock
brew cask install iina
brew cask install imageoptim
brew cask install macdown
brew cask install monodraw
brew cask install ngrok
brew cask install numi
brew cask install paw
brew cask install postico
brew cask install skype
brew cask install spotify
brew cask install textual
brew cask install transmission
brew cask install visual-studio-code

# fonts

brew tap caskroom/fonts 
brew cask install font-mononoki

brew cleanup -s
