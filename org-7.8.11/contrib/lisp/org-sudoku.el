cs lisp" . ede-proj-target-elisp)
    ("emacs lisp autoloads" . ede-proj-target-elisp-autoloads)
    ("info" . ede-proj-target-makefile-info)
    ("auxiliary" . ede-proj-target-aux)
    ("scheme" . ede-proj-target-scheme)
    ("miscellaneous" . ede-proj-target-makefile-miscelaneous)
    )
  "Alist of names to class types for available project target classes.")

(defun ede-proj-register-target (name class)
  "Register a new target class with NAME and class symbol CLASS.
This enables the creation of your target type."
  (let ((a (assoc name ede-proj-target-alist)))
    (if a
	(setcdr a class)
      (setq ede-proj-target-alist
	    (cons (cons name class) ede-proj-target-alist)))))

(defclass ede-proj-project (eieio-persistent ede-project)
  ((extension :initform ".ede")
   (file-header-line :initform ";; EDE Proj