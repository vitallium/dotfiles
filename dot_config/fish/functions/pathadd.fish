function pathadd -a dir --description "Adds a path to PATH :)"
  if test -d $dir
    set -gx PATH $dir $PATH
  end
end
