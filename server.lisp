(ql:quickload :woo)
(ql:quickload :flexi-streams)
(ql:quickload :file-types)
(ql:quickload :cl-ppcre)
(ql:quickload :cl-json)
(load "utils.lisp")

(defparameter +static-directory+ "~/arma3/static/")

(defparameter +static-prefix+ "/static/")

(defun route-hello-world (env)
  (declare (ignore env))
  '(200 (:content-type "text/plain")
    ("hello world")))

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

(defparameter *marker-info*
  '(("Terminal" "mil_start" "ColorRed" (14648.7 16756.7 0))
    ("Small Island" "mil_objective" "ColorBlue" (8443.6 25118.3 0))
    ("Molos Airfield" "mil_marker" "ColorGreen" (27096.1 24840.6 0))))

(defun route-display-post (env)
  (let* ((decoded-stream
          (flex:make-flexi-stream (getf env :raw-body) :external-format :utf-8))
         (body (read-string-stream decoded-stream)))
    (format t "~&post body:~A~%" body)
    (let ((parsed (json:decode-json-from-string body)))
      (setf *marker-info* parsed)
      (print parsed))
    (route-hello-world nil)))

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
    ("^/post-test$" ,#'route-display-post)
    (,(concatenate 'string "^" +static-prefix+ ".*$") ,#'serve-static-file)
    (nil ,#'route-not-found)))

(if (not (find-package 'swank))
    (sb-thread:make-thread
     (lambda ()
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
            (format t "method:~A uri:~A route: ~A~%"
                    (getf env :request-method)
                    (getf env :request-uri)
                    route-function)
            (funcall route-function env)))))
     :name "webserver"))

(defparameter *web* (car (sb-thread:list-all-threads)))
(list *web*)
(sb-thread:terminate-thread *web*)


