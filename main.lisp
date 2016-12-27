;; Entry point that runs the server.

;; Load the server script
(load "src/server.lisp")

;; Run the server
(serve #'hello-request-handler)
