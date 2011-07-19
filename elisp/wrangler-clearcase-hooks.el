;;; wrangler-clearcase-hooks.el 


(defvar before-commit-functions nil)

(defun mk-files-writeable (files)
  "Test the accessibility of each file, 
   and make sure every file in files
    is writeable"
  nil
  )
(defun rename-files (old-new-filename-pairs)
  "Rename old-filename to new-filename. The argument
   is a LIST of filename pairs: 
   (oldfilename, newfilename)"
  nil
  )

(defun prepare-for-commit (files-to-write old-new-filename-pairs)
 "This function prepares for Wrangler to commit 
  the changes to be made by the refactoring."
 (message-box "Files to rewite %s" files-to-write)
 (message-box "Flles to rename %s" old-new-filename-pairs)
 (mk-files-writeable files-to-write)
 (rename-files old-new-filename-pairs)
)

(add-hook 'before-commit-functions 'prepare-for-commit)

(defvar after-commit-functions nil)

(defun add-logmsg-to-logfile (logfile logmsg)
  "Add log message to the log file."
  (message-box "log msg: %s, %s" logfile logmsg)
  (write-region logmsg nil logfile 'true) 
  )

(add-hook 'after-commit-functions 'add-logmsg-to-logfile)

(provide 'wrangler-clearcase-hooks)

