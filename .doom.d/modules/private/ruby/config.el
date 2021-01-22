(set-popup-rule! "\\`\\*rspec-compilation.*?\\*\\'" :width 0.25 :side 'right :quit 'current)

(map! :mode ruby-mode
      (:leader
       (:prefix "p"
        :desc "Toggle source <=> test" :n "a" #'projectile-toggle-between-implementation-and-test))
      (:localleader
       (:prefix ("b" . "bundle"))
       (:prefix ("k" . "rake"))
       (:prefix ("r" . "robe"))
       (:prefix ("s" . "inf-ruby"))
       (:prefix ("t" . "rspec"))))

(load! "+hooks")
