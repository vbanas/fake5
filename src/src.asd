(asdf:defsystem :src
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :pathname #p"./"
  :depends-on (:src/field :src/state 
	       :src/main :src/drawer 
	       :src/parser :src/types
	       :src/server-api :src/printer
               :src/polygons :src/auto-solver
	       :src/simple-state :src/generator
	       )
  :in-order-to ((test-op (load-op :src/test/field
                                  :src/test/state
                                  :src/test/parser)))
  :perform (test-op (o c)
                    (lisp-unit:run-tests :all :src/test/field)
                    (lisp-unit:run-tests :all :src/test/state)
                    (lisp-unit:run-tests :all :src/test/parser)))

(register-system-packages :spatial-trees '(:rectangles))
