;; Memory
(defun read-memory-attr (attr meminfo)
  (read-from-string (cadr (assoc attr meminfo :test 'equal))))

;; The calculation is based off this: https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/?id=34e431b0ae398fc54ea69ff85ec700722c9da773
(defun get-used-memory ()
  (with-open-file (stream "/proc/meminfo")
    (let* ((meminfo (loop for line = (read-line stream nil)
                          while line
                          collect (let* ((attr (cl-ppcre:split "\\s*:\\s*" line))
                                         (key (car attr))
                                         (value (cdr attr)))
                                    (cons key value))))
           (used-memory-in-kb (- (read-memory-attr "MemTotal" meminfo)
                                    (read-memory-attr "MemAvailable" meminfo))))
      (floor (/ used-memory-in-kb 1000)))))

(stumpwm:add-screen-mode-line-formatter #\M 'fmt-mem)

(defun fmt-mem (ml)
  (format nil "~4D MB" (get-used-memory)))
