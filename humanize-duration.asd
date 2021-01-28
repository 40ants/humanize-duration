(defsystem "humanize-duration" 
  :class :package-inferred-system
  :author "Alexander Artemenko"
  :license "Unlicense"
  :pathname "src"
  :description "Provides HUMANIZE-DURATION function to make readable representation of LOCAL-TIME-DURATION:DURATION objects."
  :defsystem-depends-on ("mgl-pax-minimal")
  :depends-on ("humanize-duration/core"))
