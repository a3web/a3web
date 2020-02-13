(ql:quickload :woo)
(ql:quickload :flexi-streams)
(ql:quickload :file-types)
(ql:quickload :cl-ppcre)

(defparameter +static-directory+ "~/arma3/static/")

(defparameter +static-prefix+ "/static/")

(defun route-hello-world (env)
  (declare (ignore env))
  '(200 (:content-type "text/plain")
    ("hello")))

(defun route-not-found (&optional env)
  (declare (ignore env))
  '(404 (:content-type "text/plain") ("File not found")))

(defun serve-static-file (env)
  (let* ((request-path (getf env :request-uri))
         (file-path (pathname (concatenate 'string
                                           +static-directory+
                                           (subseq request-path
                                                   (length +static-prefix+))))))
    ;(format t "file path: ~A~%" file-path)
    (if (probe-file file-path)
        (let ((mime-string
               (let ((mime (file-types:file-mime file-path)))
                 (format nil "~A/~A" (nth 0 mime) (nth 1 mime)))))
          `(200 (:content-type ,mime-string) ,file-path))
        (route-not-found))))

(defun dispatch (request-path dispatch-table)
  ;; path: request url
  ;; dispatch-table: '(matcher function)
  ;;   * matcher: string or t
  ;;   * function (env) => clack-response
  (let (result)
    (dolist (route dispatch-table)
      (etypecase (car route)
        (string (if (ppcre:scan (car route) request-path)
                      (progn (setf result route)
                             (return))))
        (boolean (progn (setf result route)
                        (return)))))
    (nth 1 result)))

(defparameter +dispatch-table+
  `(("^/$" ,#'route-hello-world)
    (,(concatenate 'string "^" +static-prefix+ ".*$") ,#'serve-static-file)
    ("^/binary$"
     ,(lambda (env)
        (declare (ignore env))
        `(200
          (:content-type "text/plain")
          ,(make-array 3 :element-type '(unsigned-byte 8)
                       :initial-contents '(51 52 53)))))
    (nil nil)))
    ;;(nil ,#'route-not-found)))

(require :sb-sprof)

(sb-sprof:with-profiling
    (:max-samples 2000
                  :report :flat
                  :loop nil)
  (handler-case
      (woo:run
       (lambda (env)
         ;;(print env)
         ;;(print (type-of env))
         (if (eq :post (getf env :request-method))
             (let* ((post-stream (getf env :raw-body))
                    (char-stream (flexi-streams:make-flexi-stream
                                  post-stream
                                  :external-format :utf-8)))))
                                        ;(format t (read-line char-stream))))
                                        ;(format t "dispatching...")
         (let ((route-function (dispatch (getf env :request-uri) +dispatch-table+)))
                                        ;(format t "select route: ~A~%" route-function)
           (funcall route-function env))))
    (condition () nil)))

