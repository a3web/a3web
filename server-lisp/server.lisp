(ql:quickload :woo)
(ql:quickload :clack)
(ql:quickload :flexi-streams)
(ql:quickload :file-types)
(ql:quickload :cl-ppcre)
(ql:quickload :cl-json)
(load "utils.lisp")

(defparameter +static-directory+ "~/arma3/static/")

(defparameter +static-prefix+ "/static/")

(defun route-ok (env)
  (declare (ignore env))
  '(200 (:content-type "text/plain")
    ("ok!")))

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

(defun route-post-arma3-info (env)
  (let* ((decoded-stream
          (flex:make-flexi-stream (getf env :raw-body) :external-format :utf-8))
         (body (read-string-stream decoded-stream)))
    ;;(format t "~&post body:~A~%" body)
    (let ((parsed (json:decode-json-from-string body)))
      (setf *marker-info* (nth 0 parsed))
      (setf *units-info* (nth 1 parsed)))
      ;; (print parsed))
    (route-ok nil)))

(defparameter *units-info*
  '(("WEST" (1000.0 1000.0 0.0))
    ("EAST" (2222.0 1555.0 0.0))))

(defun route-units-pos (env)
  (declare (ignore env))
  (let* ((tagged-units-pos
          (mapcar (lambda (u)
                    (destructuring-bind (side position) u
                      `(("side" . ,side)
                        ("position" . ,position))))
                  *units-info*))
         (json (json:encode-json-to-string tagged-units-pos)))
    `(200 (:content-type "application/json")
          (,json))))

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
  `(("^/$" ,#'route-ok)
    ("^/post-arma3-info$" ,#'route-post-arma3-info)
    ("^/units-pos$" ,#'route-units-pos)
    (,(concatenate 'string "^" +static-prefix+ ".*$") ,#'serve-static-file)
    (nil ,#'route-not-found)))


(defparameter *web-server*
  (clack:clackup
   (lambda (env)
     (let ((route-function (dispatch (getf env :request-uri) +dispatch-table+)))
       (format t "method:~A uri:~A route: ~A~%"
               (getf env :request-method)
               (getf env :request-uri)
               route-function)
       (funcall route-function env)))
   :server :woo
   :use-default-middlewares nil
   :use-thread t
   :port 5000
   :address "0.0.0.0"))


(if (find-package 'swank)
    (clack:stop *web-server*))
