(ql:quickload :woo)
(ql:quickload :flexi-streams)


(with-input-from-string (s "hahah 2333333")
  (read-line s))

(woo:run
 (lambda (env)
   (print env)
   (print (type-of env))
   (if (eq :post (getf env :request-method))
       (let* ((post-stream (getf env :raw-body))
              (char-stream (flexi-streams:make-flexi-stream
                            post-stream
                            :external-format :utf-8)))
         (format t (read-line char-stream))))
   (format t "ret")
   '(200 (:content-type "text/plain") ("Hello, World"))))

(type-of 1)

