(ql:quickload '(:sicl-boot :cl-dot :cleavir-ir-visualizer))

(defun expression-to-hir (expression)
  (let* ((client (make-instance 'sicl-boot:client))
         (cst (cst:cst-from-expression expression))
         (ast (let ((cleavir-cst-to-ast::*origin* nil))
                (handler-bind
                    ((trucler:no-function-description
                       (lambda (condition)
                         (declare (ignore condition))
                         (invoke-restart 'cleavir-cst-to-ast:consider-global)))
                     (trucler:no-variable-description
                       (lambda (condition)
                         (declare (ignore condition))
                         (invoke-restart 'cleavir-cst-to-ast:consider-special))))
                  (cleavir-cst-to-ast:cst-to-ast
                   client cst sicl-boot:*e5*
                   :file-compilation-semantics nil))))
         (code-object (sicl-compiler:compile-ast client ast)))
    (sicl-compiler:ir code-object)))

(defun all-locations (instructions)
  (remove-duplicates
   (loop for instruction in instructions
         append (cleavir-ir:inputs instruction)
         append (cleavir-ir:outputs instruction)
         collect (cleavir-ir:dynamic-environment-location instruction))))

(defun view-ir (expression)
  (uiop:with-temporary-file (:pathname image-file :keep t :type "png")
    (let ((instructions
            (cleavir-ir:local-instructions-of-type
             (expression-to-hir expression)
             t)))
      (cl-dot:dot-graph
       (cl-dot:generate-graph-from-roots
        'cleavir
        (append instructions (all-locations instructions))
        '(:node (:fontname "Open Sans")
          :edge (:fontname "Open Sans")))
       image-file
       :format :png))
    (uiop:launch-program (list "eom" (uiop:native-namestring image-file)))))

(defmethod cl-dot:graph-object-node ((graph (eql 'cleavir)) (instruction cleavir-ir:instruction))
  (make-instance 'cl-dot:node
    :attributes (list :label (cleavir-ir-visualizer::label instruction)
                      :shape :box)))

(defmethod cl-dot:graph-object-node ((graph (eql 'cleavir)) (lexical cleavir-ir:lexical-location))
  (make-instance 'cl-dot:node
    :attributes (list :label (string (cleavir-ir:name lexical))
                      :shape :oval)))

(defmethod cl-dot:graph-object-node ((graph (eql 'cleavir)) (constant cleavir-ir:constant-input))
  (make-instance 'cl-dot:node
    :attributes (list :label (prin1-to-string (cleavir-ir:value constant))
                      :fillcolor "#ffcccc"
                      :style :filled
                      :shape :oval)))

(defmethod cl-dot:graph-object-node ((graph (eql 'cleavir)) (constant cleavir-ir:immediate-input))
  (make-instance 'cl-dot:node
    :attributes (list :label (prin1-to-string (cleavir-ir:value constant))
                      :fillcolor "#ccccff"
                      :style :filled
                      :shape :oval)))

(defun counting (objects attributes)
  (loop with length = (length objects)
        for n from 1
        for object in objects
        collect (make-instance 'cl-dot:attributed
                               :object object
                               :attributes (if (= length 1)
                                               attributes
                                               (list* :label (prin1-to-string n) attributes)))))

(defmethod cl-dot:graph-object-points-to ((graph (eql 'cleavir)) (instruction cleavir-ir:instruction))
  (append (counting (cleavir-ir:successors instruction)
                    '(:weight 3 :penwidth 2))
          (counting (cleavir-ir:outputs instruction)
                    '(:color "#880000"))))

(defmethod cl-dot:graph-object-pointed-to-by ((graph (eql 'cleavir)) (instruction cleavir-ir:instruction))
  (counting (cleavir-ir:inputs instruction)
            '(:color "#008800")))
