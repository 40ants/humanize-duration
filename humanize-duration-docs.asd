(defsystem "humanize-duration-docs"
  :build-pathname "docs/build/"
  :class :package-inferred-system
  :pathname "docs/source/"
  :depends-on ("humanize-duration-docs/index"))
