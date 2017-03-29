# === EDITOR ===
Pry.editor = "emacs"

# == Pry-Nav - Using pry as a debugger ==
Pry.commands.alias_command "c", "continue" rescue nil
Pry.commands.alias_command "s", "step" rescue nil
Pry.commands.alias_command "n", "next" rescue nil
Pry.commands.alias_command "r!", "reload!" rescue nil

Pry.config.color = true
Pry.config.theme = "solarized"

# === CUSTOM PROMPT ===
# This prompt shows the ruby version (useful for RVM)
Pry.prompt = [proc { |obj, nest_level, _| "#{RUBY_VERSION} (#{obj}):#{nest_level} > " }, proc { |obj, nest_level, _| "#{RUBY_VERSION} (#{obj}):#{nest_level} * " }]

Pry.config.ls.separator = "\n"
Pry.config.ls.heading_color = :magenta
Pry.config.ls.public_method_color = :green
Pry.config.ls.protected_method_color = :yellow
Pry.config.ls.private_method_color = :bright_black

# == PLUGINS ===
require "awesome_print"
require "rubygems"

AwesomePrint.pry!

Gem.path.each do |gemset|
  $:.concat(Dir.glob("#{gemset}/gems/pry-*/lib"))
end if defined?(Bundler)
$:.uniq!

Pry.config.print = proc {|output, value| Pry::Helpers::BaseHelpers.stagger_output("=> #{value.ai}", output)}

# Automatically load Pry with Rails
rails = File.join Dir.getwd, 'config', 'environment.rb'
if File.exist?(rails) && ENV['SKIP_RAILS'].nil?
  require rails
  if Rails.version[0..0] == "5"
    require 'rails/console/app'
    require 'rails/console/helpers'
  else
    warn "[WARN] cannot load Rails console commands (Not on Rails5?)"
  end

  if defined?(Rails) && Rails.env
    extend Rails::ConsoleMethods
  end
end
