;; Prettier
(add-hook! js2-mode #'prettier-js-mode)
(add-hook! rjsx-mode-hook #'prettier-js-mode)

;; Jest
(after! jest
  (setq jest-executable "yarn jest"))
(add-hook! js2-mode #'jest-minor-mode)

