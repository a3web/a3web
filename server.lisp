(ql:quickload :woo)
(ql:quickload :flexi-streams)
(ql:quickload :file-types)
(ql:quickload :cl-ppcre)

(defparameter +static-directory+ "~/arma3/static/")

(defparameter +static-prefix+ "/static/")

(defun route-hello-world (env)
  (declare (ignore env))
  '(200 (:content-type "text/plain")
    ("<!DOCTYPE html>
<html>
<head>
<title>Page Title</title>
</head>
<body>

<h1>My First Heading</h1>
<p>My first paragraph.</p>

</body>
</html>")))


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

;; --------------------------------------------
(let ((ht (make-hash-table :test 'equal)))
  (setf (gethash "abc" ht) 233)
  (gethash "abc" ht))

(defun read-file-as-binary (file-path)
  (with-open-file (stream file-path :element-type '(unsigned-byte 8))
    (let ((buffer (make-array (file-length stream)
                               :element-type '(unsigned-byte 8))))
      (read-sequence buffer stream)
      buffer)))

;; key is file-path, value is (vector '(unsigned-byte 8))
(defparameter *file-cache* (make-hash-table :test 'equal))

(defun serve-static-file-with-cache (env)
  ;; no expiration currently.
  (let* ((request-path (getf env :request-uri))
         (file-path (pathname (concatenate 'string
                                           +static-directory+
                                           (subseq request-path
                                                   (length +static-prefix+)))))
         (cache-result (gethash file-path *file-cache*)))
    (if (or (gethash file-path *file-cache*) (probe-file file-path))
        (let ((mime-string (let ((mime (file-types:file-mime file-path)))
                             (format nil "~A/~A" (nth 0 mime) (nth 1 mime)))))
          `(200 (:content-type ,mime-string)
                ,(progn (if (not (gethash file-path *file-cache*))
                            (progn (setf (gethash file-path *file-cache*)
                                         (read-file-as-binary file-path))))
                        (gethash file-path *file-cache*))))
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
    (,(concatenate 'string "^" +static-prefix+ ".*$") ,#'serve-static-file-with-cache)
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

