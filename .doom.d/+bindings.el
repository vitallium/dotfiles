(map! :leader
      (:prefix-map ("TAB" . "workspace")
       :desc "Switch to last workspace" "," #'+workspace/other))

(map! :n [mouse-8] #'better-jumper-jump-backward
      :n [mouse-9] #'better-jumper-jump-forward)
