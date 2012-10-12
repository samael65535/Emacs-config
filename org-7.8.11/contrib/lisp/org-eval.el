 from the cache.
	      (while p
		(setq c (delete (oref (car p) file) c))
		(setq p (cdr p)))
	      ;; Remove projects that aren't on the filesystem
	      ;; anymore.
	      (while c
		(when (file-exists-p (car c))
		  (setq new (cons (car c) new)))
		(setq c (cdr c)))
	      ;; Save it
	      (setq ede-project-cache-files (nreverse new))))
	(error nil))
      (when cachebuffer (kill-buffer cachebuffer))
      )))

;;; Get the cache usable.

;; @TODO - Remove this cache setup, or use this for something helpful.
;;(add-hook 'kill-emacs-hook 'ede-save-cache)
;;(when (not noninteractive)
;;  ;; No need to load the EDE cache if we aren't interactive.
;;  ;; This occurs during batch byte-compiling of other tools.
;;  (ede-load-cache))


;;; METHODS
;;
;; The methods in ede-base handle project related behavior, and DO NOT
;; related to EDE mode commands directory, such as keybindings.
;;
;; Mode related methods are in ede.el.  These methods are related
;; project specific activities not directly tied to a keybinding.
(defmethod ede-subproject-relative-path ((proj ede-project) &optional parent-in)
  "Get a path name for PROJ which is relative to the parent project.
If PARENT is specified, then be relative to the PARENT project.
Specifying PARENT is useful for sub-sub projects relative to the root project."
  (let* ((parent (or parent-in (ede-parent-project proj)))
	 (dir (file-name-directory (oref proj file))))
    (if (and parent (not (eq parent proj)))
	(file-relative-name dir (file-name-directory (oref parent file)))
      "")))

(defmethod ede-subproject-p ((proj ede-project))
  "Return non-nil if PROJ is a sub project."
  ;; @TODO - Use this in more places, and also pay attention to
  ;; metasubproject in ede-proj.el
  (ede-parent-project proj))


;;; Default descriptive methods for EDE classes
;;
;; These are methods which you might want to override, but there is
;; no need to in most situations because they are either a) simple, or
;; b) cosmetic.

;;;###autoload
(defmethod ede-name ((this ede-target))
  "Return the name of THIS target."
  (oref this name))

(defmethod ede-target-name ((this ede-target))
  "Return the name of THIS target, suitable for make or debug style commands."
  (oref this name))

(defmethod ede-name ((this ede-project))
  "Return a short-name for THIS project file.
Do this by extracting the lowest directory name."
  (oref this name))

;;;###autoload
(defmethod ede-description ((this ede-project))
  "Return a description suitable for the minibuffer about THIS."
  (format "Project %s: %d subprojects, %d targets."
	  (ede-name this) (length (oref this subproj))
	  (length (oref this targets))))

(defmethod ede-description ((this ede-target))
  "Return a description suitable for the minibuffer about THIS."
  (format "Target %s: with %d source files."
	  (ede-name this) (length (oref this source))))

;;; HEADERS/DOC
;;
;; Targets and projects are often associated with other files, such as
;; header files, documentation files and the like.  Have strong
;; associations can make useful user commands to quickly navigate
;; between the files base on their assocaitions.
;;
(defun ede-header-file ()
  "Return the header file for the current buffer.
Not all buffers need headers, so return nil if no applicable."
  (if ede-object
      (ede-buffer-header-file ede-object (current-buffer))
    nil))

(defmethod ede-buffer-header-file ((this ede-project) buffer)
  "Return nil, projects don't have header files."
  nil)

(defmethod ede-buffer-header-file ((this ede-target) buffer)
  "There are no default header files in EDE.
Do a quick check to see if there is a Header tag in this buffer."
  (with-current-buffer buffer
    (if (re-search-forward "::Header:: \\([a-zA-Z0-9.]+\\)" nil t)
	(buffer-substring-no-properties (match-beginning 1)
					(match-end 1))
      (let ((src (ede-target-sourcecode this))
	    (found nil))
	(while (and src (not found))
	  (setq found (ede-buffer-header-file (car src) (buffer-file-name))
		src (cdr src)))
	found))))

;;;###autoload
(defun ede-documentation-files ()
  "Return the documentation files for the current buffer.
Not all buffers need documentations, so return nil if no applicable.
Some projects may have multiple documentation files, so return a list."
  (if ede-object
      (ede-buffer-documentation-files ede-object (current-buffer))
    nil))

(defmethod ede-buffer-documentation-files ((this ede-project) buffer)
  "Return all documentation in project THIS based on BUFFER."
  ;; Find the info node.
  (ede-documentation this))

(defmethod ede-buffer-documentation-files ((this ede-target) buffer)
  "Check for some documentation files for THIS.
Also do a quick check to see if there is a Documentation tag in this BUFFER."
  (with-current-buffer buffer
    (if (re-search-forward "::Documentation:: \\([a-zA-Z0-9.]+\\)" nil t)
	(buffer-substring-no-properties (match-beginning 1)
					(match-end 1))
      ;; Check the master project
      (let ((cp (ede-toplevel)))
	(ede-buffer-documentation-files cp (current-buffer))))))

(defmethod ede-documentation ((this ede-project))
  "Return a list of files that provide documentation.
Documentation is not for object THIS, but is provided by THIS for other
files in the project."
  (let ((targ (oref this targets))
	(proj (oref this subproj))
	(found nil))
    (while targ
      (setq found (append (ede-documentation (car targ)) found)
	    targ (cdr targ)))
    (while proj
      (setq found (append (ede-documentation (car proj)) found)
	    proj (cdr proj)))
    found))

(defmethod ede-documentation ((this ede-target))
  "Return a list of files that provide documentation.
Documentation is not for object THIS, but is provided by THIS for other
files in the project."
  nil)

(defun ede-html-documentation-files ()
  "Return a list of HTML documentation files associated with this project."
  (ede-html-documentation (ede-toplevel))
  )

(defmethod ede-html-documentation ((this ede-project))
  "Return a list of HTML files provided by project THIS."
  
  )

;;; Default "WANT" methods.
;;
;; These methods are used to determine if a target "wants", or could
;; somehow handle a file, or some source type.
;;
(defmethod ede-want-file-p ((this ede-target) file)
  "Return non-nil if THIS target wants FILE."
  ;; By default, all targets reference the source object, and let it decide.
  (let ((src (ede-target-sourcecode this)))
    (while (and src (not (ede-want-file-p (car src) file)))
      (setq src (cdr src)))
    src))

(defmethod ede-want-file-source-p ((this ede-target) file)
  "Return non-nil if THIS target wants FILE."
  ;; By default, all targets reference the source object, and let it decide.
  (let ((src (ede-target-sourcecode this)))
    (while (and src (not (ede-want-file-source-p (car src) file)))
      (setq src (cdr src)))
    src))

(defmethod ede-target-sourcecode ((this ede-target))
  "Return the sourcecode objects which THIS permits."
  (let ((sc (oref this sourcetype))
	(rs nil))
    (while (and (listp sc) sc)
      (setq rs (cons (symbol-value (car sc)) rs)
	    sc (cdr sc)))
    rs))


;;; Debugging.
;;
;;;###autoload
(defun ede-adebug-project ()
  "Run adebug against the current EDE project.
Display the results as a debug list."
  (interactive)
  (require 'data-debug)
  (when (ede-current-project)
    (data-debug-new-buffer "*Analyzer ADEBUG*")
    (data-debug-insert-object-slots (ede-current-project) "")
    ))

;;;###autoload
(defun ede-adebug-project-parent ()
  "Run adebug against the current EDE parent project.
Display the results as a debug list."
  (interactive)
  (require 'data-debug)
  (when (ede-parent-project)
    (data-debug-new-buffer "*Analyzer ADEBUG*")
    (data-debug-insert-object-slots (ede-parent-project) "")
    ))

;;;###autoload
(defun ede-adebug-project-root ()
  "Run adebug against the current EDE parent project.
Display the results as a debug list."
  (interactive)
  (require 'data-debug)
  (when (ede-toplevel)
    (data-debug-new-buffer "*Analyzer ADEBUG*")
    (data-debug-insert-object-slots (ede-toplevel) "")
    ))


;;; Hooks & Autoloads
;;
;;  These let us watch various activities, and respond appropriately.

(add-hook 'edebug-setup-hook
	  (lambda ()
	    (def-edebug-spec ede-with-projectfile
	      (form def-body))))

(provide 'ede-base)

;;; ede-base.el ends here
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          