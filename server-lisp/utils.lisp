(defun read-file-as-binary (file-path)
  (with-open-file (stream file-path :element-type '(unsigned-byte 8))
    (let ((buffer (make-array (file-length stream)
                               :element-type '(unsigned-byte 8))))
      (read-sequence buffer stream)
      buffer)))

(defun make-dynamic-string (&optional length)
  (make-array length :adjustable t
              :fill-pointer 0
              :element-type 'character))
;; (let ((s (make-dynamic-string)))
;;   (vector-push-extend #\a s)
;;   (vector-push-extend #\b s)
;;   s)

(defmacro mvb-let* (bindings &body body)
  (let* ((exp (car bindings))
         (vars (butlast exp))
         (multi-val-exp (car (last exp)))
         (rest-bindings (cdr bindings)))
    (if rest-bindings
        `(multiple-value-bind ,vars ,multi-val-exp
           (mvb-let* ,rest-bindings ,@body))
        `(multiple-value-bind ,vars ,multi-val-exp
           ,@body))))
;; (defun test-binding-0 ()
;;   (values 1 2))
;; (defun test-binding-1 ()
;;   (values 1 2 3))
;; (mvb-let* ((a b (test-binding-0))
;;            (c d e (test-binding-1)))
;;   (list a b c d e))

(defun read-string-stream (stream)
  (let* ((buffer (make-string 5))
         (result-list '()))
    (do ((read-count (read-sequence buffer stream)
                     (read-sequence buffer stream)))
        ((= read-count 0) nil)
      (push (subseq buffer 0 read-count) result-list))
    (format nil "~{~a~}" (reverse result-list))))
;; (with-input-from-string (s "0123456789012")
;;   (read-string-stream s))
