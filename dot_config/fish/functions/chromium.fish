function chromium.acch --description "activate chromium build mode"
    pathadd "$HOME/.local/bin/depot_tools"
end

function chromium.deps --description "run gclient sync"
    env GYP_DEFINES=disable_nacl=1 gclient sync --jobs=70
end

function chromium.hooks --description "run gclient runhooks"
    env GYP_DEFINES=disable_nacl=1 gclient runhooks
end
