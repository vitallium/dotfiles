#!/bin/bash

asdf plugin-add python
asdf plugin-add nodejs
asdf plugin-add ruby
asdf plugin-add erlang
asdf plugin-add elixir

asdf install
asdf reshim
