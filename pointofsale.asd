; Haha, I'm not an ASDF system definition! :)
; It's just a loader script to properly initialize all of the parts of application.
; To be honest, it's not necessarily needed to separate such a small program into several files 
; but it's a convention helping scale program well.

(ql:quickload :stefil)

(load (merge-pathnames "package.lisp"))
(load (merge-pathnames "code.lisp"))
(load (merge-pathnames "tests.lisp"))

