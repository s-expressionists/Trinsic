(asdf:defsystem "trinsic"
  :description "Common Lisp utility system to aid in extrinsic and intrinsic system construction."
  :license "MIT"
  :author "Tarn W. Burton"
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Trinsic"
  :bug-tracker "https://github.com/s-expressionists/Trinsic/issues"
  :components ((:module "code"
                :serial t
                :components ((:file "packages")
                             (:file "interface")))))
