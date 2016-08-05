(asdf:defsystem :src
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :pathname #p"./"
  :depends-on (:src/field :src/state :src/main :src/drawer :src/parser)
>>>>>>> 1d85b93766a4c7b699c0547f047ecc54f179c762
  :in-order-to ((test-op (load-op :src/test/field
                                  :src/test/state)))
  :perform (test-op (o c)
                    (lisp-unit:run-tests :all :src/test/field)
                    (lisp-unit:run-tests :all :src/test/state)))

(register-system-packages :spatial-trees '(:rectangles))
