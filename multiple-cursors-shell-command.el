;;; multiple-cursors-shell-command.el --- Run shell command for each cursor in multiple-cursors mode -*- lexical-binding: t; -*-

;; Author: Hauke Rehfeld <emacs@haukerehfeld.de>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (multiple-cursors "1.4.0"))
;; Keywords: convenience, tools, shell
;; URL: https://github.com/hrehfeld/emacs-multiple-cursors-shell-command

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides a function to run a shell command for each cursor in
;; multiple-cursors mode. See `multiple-cursors-shell-command'.

;;; Code:

(require 'multiple-cursors)

(defun multiple-cursors-shell-command (command &optional arg)
  "Run a shell COMMAND for each cursor in multiple-cursors mode. See `shell-command'.

With optional prefix ARG, insert output at point, just like `shell-command'."
  (interactive
   ;; Straight from shell-command
   (list (read-shell-command (if shell-command-prompt-show-cwd
                                 (format-message "Shell command in `%s': "
                                                 (abbreviate-file-name
                                                  default-directory))
                               "Shell command: ")
                             nil nil
                             (let ((filename
                                    (cond
                                     (buffer-file-name)
                                     ((eq major-mode 'dired-mode)
                                      (dired-get-filename nil t)))))
                               (and filename (file-relative-name filename))))
         current-prefix-arg))
  (mc/for-each-cursor-ordered
   (save-excursion
     (goto-char (overlay-start cursor))
     (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
       (setq prefix-arg arg)
       (shell-command (concat command " " (shell-quote-argument line)))))))

(provide 'multiple-cursors-shell-command)

;;; multiple-cursors-shell-command.el ends here
