e-directory-p sd)
		   (ede-directory-project-p sd))
	      (oset ret subproj
		    (cons (ede-proj-load sd (or rootproj ret))
			  (oref ret subproj))))
	  (setq subdirs (cdr subdirs))))
      ret)))

(defun ede-proj-save (&optional project)
  "Write out object PROJECT into its file."
  (save-excursion
    (if (not project) (setq project (ede-current-project)))
    (let ((cdir (oref project directory)))
      (unwind-protect
	  (progn
	    (slot-makeunbound project :directory)
	    (eieio-persistent-save project))
	;; Restore the directory slot
 	(oset project directory cdir))) ))

(defmethod ede-commit-local-variables ((proj ede-proj-project))
  "Commit change to local variables in PROJ."
  (ede-proj-save proj))

(defmethod eieio-done-customizing ((proj ede-proj-project))
  "Call this when a user finishes customizing this object.
Argument PROJ is the project to save."
  (call-next-method)
  (ede-proj-save proj))

(defmethod eieio-done-customizing ((target ede-proj-target))
  "Call this when a user finishes customizing this object.
Argument TARGET is the project we are completing customization on."
  (call-next-method)
  (ede-proj-save (ede-current-project)))

(defmethod ede-commit-project ((proj ede-proj-project))
  "Commit any change to PROJ to its file."
  (ede-proj-save proj))

(defmethod ede-buffer-mine ((this ede-proj-project) buffer)
  "Return t if object THIS lays claim to the file in BUFFER."
  (let ((f (ede-convert-path this (buffer-file-name buffer))))
    (or (string= (file-name-nondirectory (oref this file)) f)
	(string= (ede-proj-dist-makefile this) f)
	(string-match "Makefile\\(\\.\\(in\\|am\\)\\)?$" f)
	(string-match "config\\(ure\\.\\(in\\|ac\\)\\|\\.status\\)?$" f)
	(string-match "config.h\\(\\.in\\)?" f)
	(member f '("AUTHORS" "NEWS" "COPYING" "INSTALL" "README"))
	)))

(defmethod ede-buffer-mine ((this ede-proj-target) buffer)
  "Return t if object THIS lays claim to the file in BUFFER."
  (or (call-next-method)
      (ede-target-buffer-in-sourcelist this buffer (oref this auxsource))))


;;; EDE command functions
;;
(defvar ede-proj-target-history nil
  "History when querying for a target type.")

(defmethod project-new-target ((this ede-proj-project)
			       &optional name type autoadd)
  "Create a new target in THIS based on the current buffer."
  (let* ((name (or name (read-string "Name: " "")))
	 (type