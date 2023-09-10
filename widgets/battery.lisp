(defun get-battery-capacity ()
  (with-open-file (stream "/sys/class/power_supply/BAT0/capacity")
    (read-line stream)))

(stumpwm:add-screen-mode-line-formatter #\B 'fmt-battery)

(defun fmt-battery (ml)
  (format nil "~4D %" (get-battery-capacity)))
