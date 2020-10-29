;; Magit
(use-package magit)

;; Enable delta in Magit
(use-package magit-delta
  :after magit)

;; Enable Evil support for Magit
(use-package evil-magit
  :after evil magit)

(provide 'init-vc)

;;; init-vc.el ends here
