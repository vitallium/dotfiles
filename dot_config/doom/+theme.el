;;; $DOOMDIR/+theme.el -*- lexical-binding: t; -*-

;; In Emacs > 29, we can use Po Lu’s pixel-scroll-precision-mode to get a faster and better scrolling.
(when *is-emacs-29*
  (pixel-scroll-precision-mode))

;; From https://github.com/gf3/dotfiles/
(defun gf3/get-dpi (&optional frame)
  "Get the DPI of FRAME (or current if nil)."
  (cl-flet ((pyth (lambda (w h)
                    (sqrt (+ (* w w)
                             (* h h)))))
            (mm2in (lambda (mm)
                     (/ mm 25.4))))
    (let* ((atts (frame-monitor-attributes frame))
           (pix-w (cl-fourth (assoc 'geometry atts)))
           (pix-h (cl-fifth (assoc 'geometry atts)))
           (pix-d (pyth pix-w pix-h))
           (mm-w (cl-second (assoc 'mm-size atts)))
           (mm-h (cl-third (assoc 'mm-size atts)))
           (mm-d (pyth mm-w mm-h)))
      (/ pix-d (mm2in mm-d)))))

(defun gf3/preferred-font-size ()
  "Calculate the preferred font size based on the monitor DPI."
  (let ((dpi (gf3/get-dpi)))
    (cond
     ((< dpi 110) 14.0)
     ((< dpi 130) 16.0)
     ((< dpi 160) 18.0)
     (t 16.0))))

(use-package! modus-themes
  :custom
  (modus-themes-mode-line '(accented))
  (modus-themes-italic-constructs t))

(after! doom-themes
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(after! treemacs
  (add-hook! 'treemacs-mode-hook #'treemacs-follow-mode)
  (setq doom-themes-treemacs-enable-variable-pitch nil))

(setq
 ;; Fonts
 ;; Primary font to use
 doom-font (font-spec :family "Berkeley Mono" :size (gf3/preferred-font-size))
 ;; Non-monospace font
 doom-variable-pitch-font (font-spec :family "iA Writer Duo S" :size (gf3/preferred-font-size))
 ;; For big-font-mode
 doom-big-font (font-spec :family "mononoki" :size 20.0)
 ;; For unicode glyphs
 ;; (doom-unicode-font)
 ;; For `fixed-pitch-serif' face
 ;; (doom-serif-font)
 ;; Theme
 doom-theme 'ef-day
 ;; Disable line numbers
 display-line-numbers-type nil
 doom-font-increment 1.0
 window-resize-pixelwise t)

(after! doom-modeline
  (setq doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-file-name-style 'truncate-with-project))

(when IS-MAC
  (setq frame-title-format nil)
  (dolist (filter '((ns-transparent-titlebar . t)
                    (ns-appearance . unbound)))
    (cl-pushnew filter default-frame-alist :test #'equal)))
