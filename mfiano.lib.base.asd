(asdf:defsystem #:mfiano.lib.base
  :description "Base utilities for all of mfiano's projects"
  :author "Michael Fiano <michael.fiano@gmail.com>"
  :license "MIT"
  :homepage "https://github.com/mfiano/mfiano.lib.base"
  :encoding :utf-8
  :depends-on
  (#:alexandria
   #:uiop)
  :pathname "src"
  :serial t
  :components
  ((:file "pkg/package")
   (:file "util/package")
   (:file "util/constants")
   (:file "util/types")
   (:file "util/macros")
   (:file "util/list")
   (:file "util/array")
   (:file "util/character")
   (:file "util/string")
   (:file "util/symbol")
   (:file "util/hash-table")
   (:file "util/file")
   (:file "util/numbers")
   (:file "util/bit-ops")))
