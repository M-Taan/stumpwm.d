(in-package :stumpwm)

;;; Defaults
(set-prefix-key (kbd "C-z"))
(setf *startup-message* nil)
;; Mainly due to emacs's resize hints
(setf *ignore-wm-inc-hints* t
      *mouse-focus-policy* :click)

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

;;; Startup
(run-shell-command "/usr/local/bin/emacs --daemon")
(run-shell-command "nitrogen --restore")
(run-shell-command "dunst")

;;; Binding
;; Window classes are set by Xorg, you can get the window class through: (window-class window)
;; or through xorg's: xprop WM_CLASS
(defcommand emacsclient () ()
            (run-or-raise "emacsclient -c" '(:class "Emacs")))

(defcommand firefox () ()
            (run-or-raise "firefox" '(:class "firefox")))

(defcommand slack () ()
            (run-or-raise "slack" '(:class "Slack")))

(define-frame-preference "[EMACS]"
    (0 t t :class "Emacs"))

(define-frame-preference "[BROWSER]"
    (0 t t :class "firefox"))

(define-frame-preference "[SLACK]"
    (0 t t :class "Slack"))

(setf *applications-keymap*
      (let ((m (make-sparse-keymap)))
        (define-key m (kbd "b") "firefox")
        (define-key m (kbd "s") "slack")
        (define-key m (kbd "a") "exec pavucontrol")
        (define-key m (kbd "r") "exec rofi -show drun")
        m))

(define-key *root-map* (kbd "a") '*applications-keymap*)
(define-key *root-map* (kbd "e") "emacsclient")
(define-key *root-map* (kbd "c") "exec alacritty")

;; Top level mappings
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "exec amixer -D pulse sset Master 5%+")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "exec amixer -D pulse sset Master 5%-")
(define-key *top-map* (kbd "XF86AudioPlay") "exec playerctl --all-players play")
(define-key *top-map* (kbd "XF86AudioPause") "exec playerctl --all-players pause")
(define-key *top-map* (kbd "XF86AudioNext") "exec playerctl --all-players next")
(define-key *top-map* (kbd "XF86AudioPrev") "exec playerctl --all-players previous")
(define-key *top-map* (kbd "XF86MonBrightnessUp") "exec light -A 10")
(define-key *top-map* (kbd "XF86MonBrightnessDown") "exec light -U 10")
(define-key *top-map* (kbd "Print") "exec screenshot")
(define-key *top-map* (kbd "C-M-Delete") "exec betterlockscreen --lock")

;; undefine keys
(undefine-key *root-map* (kbd "C-e"))
(undefine-key *root-map* (kbd "C-c"))

;;; Modeline
(load (concat (getenv "HOME") "/.stumpwm.d/util/memory.lisp"))
(load (concat (getenv "HOME") "/.stumpwm.d/util/battery.lisp"))

(setf *time-modeline-string* "%a %b %e %l:%M %p"
      *mode-line-pad-y* 4
      *mode-line-timeout* 2
      *screen-mode-line-format* (list "%g" "^>" "%M" " | "  "%B"  " | " "%d"))
{
(dolist (head (screen-heads (current-screen)))
  (enable-mode-line (current-screen) head t *screen-mode-line-format*))

(set-font "-*-terminal-medium-r-*-*-*-*-*-*-*-iso8859-*")

(defun float-emulator (window)
  (when (title-re-p window "^Emulator$")
    (focus-window window)
    (float-this)))

(when *initializing*
  (add-hook *new-window-hook* (lambda (window) (float-emulator window))))

(defun flatten-group (group)
  (dolist (window (group-windows group))
    (when (typep window 'float-window)
      (unfloat-window window group))))

(defun flatten-groups ()
  (dolist (group (screen-groups (current-screen)))
    (flatten-group group)))

(defun connect-to-home-screen ()
  (flatten-groups)
  (run-shell-command "connect-to-home-screen"))

(defun disconnect-screen ()
  (flatten-groups)
  (run-shell-command "xrandr --output eDP 2560x1600 --rate 120 --primary"))

(defcommand hsplit-focus () ()
            (hsplit)
            (fnext))

(defcommand vsplit-focus () ()
            (vsplit)
            (fnext))

(define-key *root-map* (kbd "S") "hsplit-focus")
(define-key *root-map* (kbd "s") "vsplit-focus")
