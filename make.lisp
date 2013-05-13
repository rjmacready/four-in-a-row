
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload 'lispbuilder-sdl)
  (ql:quickload 'cffi))

(load (compile-file "main.lisp"))


(defun main()
  ; Load SDL
  (cffi:define-foreign-library sdl
      (:unix "libSDL.so"))

  (cffi:use-foreign-library sdl)

  ; Run Game
  (play)

  ; Quit
  #+sbcl (exit) 
  #-sbcl (error "Die, you infidel!!!")
  )

(let ((filename #+windows "main.exe" #-windows "main"))
      #+sbcl (save-lisp-and-die filename :toplevel #'main :executable t)
      #-sbcl (error "Die, you infidel!!!")
    )
