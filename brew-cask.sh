#!/bin/bash

brew install caskroom/cask/brew-cask

# must have
brew cask install 1password
brew cask install alfred
brew cask install tower
brew cask install beyond-compare
brew cask install google-chrome
brew cask install iterm2
brew cask install caffeine
brew cask install forklift

# optional
brew cask install acorn
brew cask install charles
brew cask install daisydisk
brew cask install dash
brew cask install firefox
brew cask install hyperdock
brew cask install hyperswitch
brew cask install iina
brew cask install grammarly
brew cask install imageoptim
brew cask install macdown
brew cask install monodraw
brew cask install ngrok
brew cask install numi
brew cask install paw
brew cask install postico
brew cask install skype
brew cask install textual
brew cask install transmission
brew cask install visual-studio-code

# work stuff
brew cask install avocode
brew cask install mattermost
brew cask install sketch

# fonts

brew tap caskroom/fonts 
brew cask install font-mononoki

brew cask cleanup
