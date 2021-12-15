(defun compile-in-sicl (form)
  (expression-to-hir form sicl-boot:*e5*))

(defvar *block-names* '())
(defvar *variable-names* '())
(defvar *tag-names* '())
(defvar *depth* 0)

(defun make-form ()
  (when (> *depth* 8)
    (return-from make-form (random 20)))
  (let ((*depth* (1+ *depth*)))
    (ecase (random 5)
      (0 (case (random 5)
           (0 `(if ,(make-form)
                   ,(make-form)
                   ,(make-form)))
           (1 (let* ((block-name (gensym))
                     (*block-names* (cons block-name *block-names*)))
                `(block ,block-name
                   ,(make-form))))
           (2 (let* ((tags (loop repeat (1+ (random 3)) collect (gensym)))
                     (*tag-names* (append tags *tag-names*)))
                `(tagbody
                    ,@(loop for tag in tags
                            collect tag
                            collect (make-form)))))
           (3 (let ((variable-name (gensym)))
                `(let ((,variable-name ,(make-form)))
                   ,(let ((*variable-names*
                            (cons variable-name *variable-names*)))
                      (make-form)))))
           (4 (let ((variable-name (gensym)))
                `(lambda (,variable-name) ,(make-form))))))
      ((1 2) (if (null *variable-names*)
                 (make-form)                  ; try again
                 (alexandria:random-elt *variable-names*)))
      ((3) (if (null *block-names*)
               (make-form)
               `(return-from ,(alexandria:random-elt *block-names*)
                  ,(make-form))))
      ((4) (if (null *tag-names*)
               (make-form)
               `(go ,(alexandria:random-elt *tag-names*)))))))

(defun fuzz ()
  (loop
    (let* ((*gensym-counter* 0)
           (form (make-form)))
      (handler-case (compile-in-sicl form)
        (error (e)
          (format t "Compiler signalled~%  ~a~%while compiling~%  ~a"
                  e
                  form)
          (return))))))
