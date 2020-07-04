# Editor {{{
Pry.editor = "nvim"
# }}}

# Pry-Nav {{{
Pry.commands.alias_command "c", "continue" rescue nil
Pry.commands.alias_command "s", "step" rescue nil
Pry.commands.alias_command "n", "next" rescue nil
Pry.commands.alias_command "r!", "reload!" rescue nil
# }}}

# Plugins {{{
begin
  require 'awesome_print'
  Pry.config.print = proc {|output, value| Pry::Helpers::BaseHelpers.stagger_output("=> #{value.ai}", output)}
rescue LoadError => err
  puts 'gem install awesome_print  # <-- highly recommended'
end

begin
  require 'hirb'
  Hirb.enable
end

if Pry::Prompt[:rails]
  Pry.config.prompt = Pry::Prompt[:rails][:value]
end
# }}}

# Pry configuration {{{
Pry.config.color = true
Pry.config.history.file = File.join(__dir__, '.pry_history')
# }}}

# vim:fdm=marker
