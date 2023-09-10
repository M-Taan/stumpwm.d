(in-package :stumpwm)

;; Setting default prefix
(set-prefix-key (kbd "C-z"))

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
