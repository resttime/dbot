(in-package #:dbot)

(defparameter *active* nil)
(defbot *bot* "")

(defun test (channel)
  (setf *active* t)
  (connect *bot*)
  (loop while *active* do
    (let* ((log (uiop:process-info-output *server*))
           (output (format nil "~{~a~%~}"
                           (loop while (listen log)
                                 for line = (read-line log)
                                 for result = (filter-log line)
                                 when result
                                   collect result))))
      (sleep 1)
      (unless (string= output "")
        (princ output)
        (create output (from-id channel :channel)))))
  (disconnect *bot*))

