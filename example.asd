(defsystem "example" 
  :class :package-inferred-system
  :author "Alexander Artemenko"
  :license "Unlicense"
  :pathname "src"
  :description "This description will be used only if long-description is missings."
  :depends-on ("mgl-pax"
               "example/app"))
