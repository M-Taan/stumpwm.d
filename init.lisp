(in-package :stumpwm)

;; Setting default prefix
(set-prefix-key (kbd "C-z"))

;; defaults
(setf *startup-message* nil)

;;; Groups

;; The goal is to always find what I'm looking for
;; The flow will work as follows:
;; Group 1 (Tiling) Browser
;; Group 2 (Tiling) Emacs
;; Group 3 (Tiling) Terminal
;; Group 4 (Tiling) will be empty by default, it's mainly for misc apps
;; Group 5 (Tiling) Slack
(grename "[EMACS]")
(gnewbg "[BROWSER]")
(gnewbg "[TERM]")
(gnewbg "[MISC]")
(gnewbg "[SLACK]")

;; Startup
(run-shell-command "/usr/local/bin/emacs --daemon")
(run-shell-command "nitrogen --restore")
(run-shell-command "dunst")

;; Binding
;; Window classes are set by Xorg, you can get the window class through: (window-class window)
;; or through xorg's: xprop WM_CLASS
(defcommand emacsclient () ()
            (run-or-raise "emacsclient -c" '(:class "Emacs")))

(defcommand firefox () ()
            (run-or-raise "firefox" '(:class "firefox")))

(setf *applications-keymap*
      (let ((m (make-sparse-keymap)))
        (define-key m (kbd "b") "firefox")
        m))

(define-key *root-map* (kbd "a") '*applications-keymap*)
(define-key *root-map* (kbd "e") "emacsclient")

;;; Modeline
(load (concat (getenv "HOME") "/.stumpwm.d/widgets/memory.lisp"))
(load (concat (getenv "HOME") "/.stumpwm.d/widgets/battery.lisp"))

(setf *time-modeline-string* "%a %b %e %l:%M %p"
      *mode-line-pad-y* 4
      *mode-line-timeout* 2
      *screen-mode-line-format* (list "%g" "^>" "%M" " | "  "%B"  " | " "%d"))

(dolist (head (screen-heads (current-screen)))
  (enable-mode-line (current-screen) head t *screen-mode-line-format*))

(set-font "-*-terminal-medium-r-*-*-*-*-*-*-*-iso8859-*")
