(in-package #:dbot)

(defparameter *base-dir* #p"~/factorio/")
(defparameter *program* #p"~/factorio/bin/x64/factorio")

(defparameter *config-dir* #p"~/.config/factorio/")
(defparameter *save* #p"~/.config/factorio/my-save.zip")
(defparameter *settings* #p"~/.config/factorio/server-settings.json")

(defvar *server* nil)

(defun start-server ()
  (setf *server*
        (uiop:launch-program
         '(*program* "--start-server" *save* "--server-settings" *settings*)
         :input :stream :output :stream)))

(defun stop-server ()
  (format t "INFO: Attempting to stop server~%")
  (if *server*
      (if (uiop:terminate-process *server*)
          (format t "INFO: Server Stopped~%")
          (format t "WARN: Failed to stop server.  Server might be already stopped~%"))
      (format t "WARN: No server to stop~%")))

(defun filter-log (line)
  (cl-ppcre:register-groups-bind (type user msg)
      ("\\[(JOIN|CHAT|LEAVE)\\] <?(\\w+)>?:? (.*)" line)
    (alexandria:switch (type :test #'equal)
      ("JOIN" (format nil "*~a ~a*"  user msg))
      ("LEAVE" (format nil "*~a ~a*"  user msg))
      ("CHAT" (format nil "**<~a>**: ~a" user msg)))))
;; (filter-log "2020-05-01 04:42:45 [JOIN] player1 joined the game")
;; (filter-log "2020-05-01 04:42:45 [CHAT] player1: hi")
;; (filter-log "2020-05-01 04:42:45 [CHAT] <server>: hi")

(defun get-factorio-version ()
  (let ((version (uiop:run-program `(,*program* "--version") :output :string)))
    (ppcre:scan-to-strings "(\\d+\\.\\d+\\.\\d+)" version)))

(defun get-update-list ()
  (let* ((url "https://updater.factorio.com/get-available-versions")
         (json (jsown:parse (dex:get url)))
         (updates (jsown:val json "core-linux_headless64")))
    (loop for u in updates
          when (find "from" (jsown:keywords u) :test #'string=)
            collect `(from ,(jsown:val u "from")
                           to ,(jsown:val u "to")))))

(defun update-checker (version)
  (lambda (update)
    (let ((from-version (getf update 'from)))
      (string= from-version version))))

(defun update-for (version)
  (let* ((update-list (get-update-list))
         (update (find-if (update-checker version) update-list)))
    update))

(defun get-download-link (update)
  (let* ((from-version (getf update 'from))
         (to-version (getf update 'to))
         (base-url "https://updater.factorio.com/get-download-link?package=core-linux_headless64")
         (update-url (format nil "~a&from=~a&to=~a" base-url from-version to-version))
         (json (jsown:parse (dex:get update-url)))
         (download-link (first json)))
    download-link))

(defun download (link destination)
  (let* ((download-stream (dex:get link :want-stream t))
         (buffer-size (* 16 1024)) ; Using CURL's default buffer size
         (buffer (make-array buffer-size :element-type 'unsigned-byte)))
    (with-open-file (out destination
                         :direction :output
                         :if-exists nil
                         :if-does-not-exist :create
                         :element-type 'unsigned-byte)
      (if out
          (progn
            (format t "Downloading: ~a~% link")
            (format t "Download destination: ~a~%exit destination")
            (loop :for p := (read-sequence buffer download-stream)
                  :while (plusp p)
                  :do (write-sequence buffer out :end p)))
          (format t "Destination already exists: ~a" destination))
      destination)))

(defun download-update (update)
  (let* ((link (get-download-link update))
         (filename (ppcre:scan-to-strings "(core-linux_headless64.*\\.zip)" link))
         (destination (merge-pathnames *config-dir* filename)))
    (download link destination)))

(defun apply-update (update-location)
  (uiop:run-program `(,*program* "--apply-update" ,(namestring update-location))
                    :output t))

(defun do-update ()
  (let* ((current-version (get-factorio-version))
         (update (update-for current-version)))
    (when update
      (apply-update (download-update update)))))
