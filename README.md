# J. B. TDD training exercise 2 - Point of Sale

## How to run the tests

Assuming absolutely freshly installed implementation of Common Lisp (tested on SBCL and ECL) with no Quicklisp,
one have to do the following (exactly) to get to the tests (CL implementation arbitrarily chosen to be SBCL):

    $ cd /this/project/dir
    $ wget http://beta.quicklisp.org/quicklisp.lisp
    $ sbcl
    * (load "quicklisp.lisp")
    * (quicklisp-quickstart:install :path "/desired/path/for/quicklisp/with/trailing/slash/")
    * (load "pointofsale.asd")
    * (point-of-sale:all-tests)
    
This has a side effect of Quicklisp package manager being installed to your system (locally, and to where you say it).
Quicklisp is needed for Stefil testing framework to work.

After the `(load "pointofsale.asd")` you can do the following:

    * (in-package :point-of-sale)
    
And you can fiddle with the system at your heart's content, using the following braindead API:

    * (cost-of "barcode_here" is "price_here")
    * (on-barcode "barcode_here")
    * (emitted-string) # will print the "price_here" to REPL
    
