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

(defun vitallium/reload-font-size ()
  "Reload font size according to the DPI"
  (interactive)
  (setq
   doom-font (font-spec :family "Berkeley Mono" :size (gf3/preferred-font-size))
   doom-variable-pitch-font (font-spec :family "iA Writer Duo S" :size (gf3/preferred-font-size))
   doom-big-font (font-spec :family "Berkeley Mono" :size (* 1.5 (gf3/preferred-font-size))))
  (doom/reload-font))

(after! modus-themes
  (setq modus-themes-org-blocks 'gray-background
        modus-themes-slanted-constructs t
        modus-themes-bold-constructs t
        modus-themes-variable-pitch-ui t
        modus-themes-prompts '(italic semibold)))

(after! ef-themes
  (setq ef-themes-variable-pitch-ui t
        ef-themes-to-toggle '(ef-day ef-night)))

(after! doom-themes
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  (doom-themes-treemacs-config))

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
 doom-big-font (font-spec :family "Berkeley Mono" :size (* 1.5 (gf3/preferred-font-size)))
 ;; For unicode glyphs
 ;; (doom-unicode-font)
 ;; For `fixed-pitch-serif' face
 ;; (doom-serif-font)
 ;; Theme
 doom-theme 'modus-operandi
 ;; Disable line numbers
 display-line-numbers-type nil
 doom-font-increment 1.0
 window-resize-pixelwise t)

(after! doom-modeline
  (setq doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-file-name-style 'truncate-with-project))

(when IS-MAC
  (dolist (filter '((ns-transparent-titlebar . t)
                    (ns-appearance . unbound)))
    (cl-pushnew filter default-frame-alist :test #'equal)))

(use-package! auto-dark
  :custom
  (auto-dark-dark-theme 'modus-vivendi)
  (auto-dark-light-theme 'modus-operandi)
  :config
  (auto-dark-mode t))
