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

;; Battery
;; This's just to get things going for now
;; Need to handle this better
;; It's either create your own batter module
;; Or use the already available battery module (battery-portable)
(defun get-battery-capacity ()
  (with-open-file (stream "/sys/class/power_supply/BAT0/capacity")
    (read-line stream)))

;; Memory
(defun read-memory-information ()
  (with-open-file (stream "/proc/meminfo")
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(setf *time-modeline-string* "%a %b %e%l:%M %p"
      *mode-line-pad-y* 4
      *mode-line-timeout* 2
      *screen-mode-line-format* (list "%g" "^>"'(:eval (get-battery-capacity)) "%" " | " "%d"))

(toggle-mode-line (current-screen) (current-head))

(set-font "-*-terminal-medium-r-*-*-*-*-*-*-*-iso8859-*")
