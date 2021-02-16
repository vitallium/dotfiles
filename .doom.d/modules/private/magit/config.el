(setq magit-repository-directories '(("~/projects/" . 3))
      magit-save-repository-buffers nil
      magit-display-file-buffer-function #'switch-to-buffer-other-window
      magithub-clone-default-directory "~/projects"
      magithub-preferred-remote-method 'ssh_url
      magit-diff-refine-hunk 'all)

(load! "+hooks")
