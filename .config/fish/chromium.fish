function deps --description "run gclient sync"
  env GYP_DEFINES=disable_nacl=1 gclient sync --jobs=70
end

function hooks --description "run gclient runhooks"
  env GYP_DEFINES=disable_nacl=1 gclient runhooks
end

function actch --description "activate chromium build mode"
  set fish_user_paths "$HOME/.bin/depot_tools" $fish_user_paths
end
