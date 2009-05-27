;;; erl-service.el --- High-level calls to Erlang services.

;;;; Frontmatter
;;
;; This module implements Emacs commands - i.e. M-x'able key-bind'able
;; sort - for doing stuff with Erlang nodes.
;;
;; The general implementation strategy is to make RPCs to the "distel"
;; erlang module, which does most of the work for us.

(require 'erlang)
(eval-when-compile (require 'cl))
(require 'werl)

;;;; Base framework

;;;;; Target node

(defvar erl-nodename-cache nil
  "The name of the node most recently contacted, for reuse in future
commands. Using C-u to bypasses the cache.")

(defvar erl-nodename-history nil
  "The historical list of node names that have been selected.")

(defun erl-target-node ()
  "Return the name of the default target node for commands.
Force node selection if no such node has been choosen yet, or when
invoked with a prefix argument." 
  (or (and (not current-prefix-arg) erl-nodename-cache)
      (erl-choose-nodename)))

(defun erl-set-cookie ()
  "Prompt the user for the cookie."
  (interactive)
  (let* ((cookie (read-string "Cookie: ")))
    (if (string= cookie "")
        (setq derl-cookie nil)
      (setq derl-cookie cookie))))

(defun erl-get-cookie ()
  "Print the cookie."
  (interactive)
  (message "Cookie: %s" derl-cookie))

(defun erl-choose-nodename ()
  "Prompt the user for the nodename to connect to in future."
  (interactive)
  (let* ((nodename-string (if erl-nodename-cache
			      (symbol-name erl-nodename-cache)
			    nil))
	 (name-string (read-string (if nodename-string
				       (format "Node (default %s): "
					       nodename-string)
				     "Node: ")
				   nil
				   'erl-nodename-history
				   nodename-string))
         (name (intern (if (string-match "@" name-string)
                           name-string
			 (concat name-string
				 "@" (erl-determine-hostname))))))
    (when (string= name-string "")
      (error "No node name given"))
    (setq erl-nodename-cache name)
    (setq distel-modeline-node name-string)
    (force-mode-line-update))
  erl-nodename-cache)

;;;;; Call MFA lookup

(defun erl-read-call-mfa ()
  "Read module, function, arity at point or from user.
Returns the result in a list: module and function as strings, arity as
integer."
  (interactive) ; for testing
  (let* ((mfa-at-point (erl-mfa-at-point))
         (mfa (if (or (null mfa-at-point)
                      current-prefix-arg
                      distel-tags-compliant)
                  (erl-parse-mfa 
		   (read-string 
		    "Function reference: "
		    (if current-prefix-arg nil (erl-format-mfa mfa-at-point))))
                mfa-at-point)))
    mfa))

(defun erl-format-mfa (mfa)
  "Format (MOD FUN ARITY) as MOD:FUN/ARITY.
If MFA is nil then return nil.
If only MOD is nil then return FUN/ARITY."
  (if mfa
      (destructuring-bind (m f a) mfa
        (if m (format "%s:%s/%S" m f a) (format "%s/%S" f a)))))

(defun erl-parse-mfa (string &optional default-module)
  "Parse MFA from a string using `erl-mfa-at-point'."
  (when (null default-module) (setq default-module (erl-buffer-module-name)))
  (with-temp-buffer
    (with-syntax-table erlang-mode-syntax-table
      (insert string)
      (goto-char (point-min))
      (erl-mfa-at-point default-module))))

(defun erl-buffer-module-name ()
  "Return the current buffer's module name, or nil."
  (erlang-get-module))

(defun erl-mfa-at-point (&optional default-module)
  "Return the module, function, arity of the function reference at point.
If not module-qualified then use DEFAULT-MODULE."
  (when (null default-module) (setq default-module (erl-buffer-module-name)))
  (save-excursion
    (erl-goto-end-of-call-name)
    (let ((arity (erl-arity-at-point))
	  (mf (erlang-get-function-under-point)))
      (if (null mf)
	  nil
        (destructuring-bind (module function) mf
          (list (or module default-module) function arity))))))

;;; FIXME: Merge with erlang.el!
(defun erl-arity-at-point ()
  "Get the number of arguments in a function reference.
Should be called with point directly before the opening ( or /."
  ;; Adapted from erlang-get-function-arity.
  (save-excursion
    (cond ((looking-at "/")
	   ;; form is /<n>, like the /2 in foo:bar/2
	   (forward-char)
	   (let ((start (point)))
	     (if (re-search-forward "[0-9]+" nil t)
                 (ignore-errors (car (read-from-string (match-string 0)))))))
	  ((looking-at "[\n\r ]*(")
	   (goto-char (match-end 0))
	   (condition-case nil
	       (let ((res 0)
		     (cont t))
		 (while cont
		   (cond ((eobp)
			  (setq res nil)
			  (setq cont nil))
			 ((looking-at "\\s *)")
			  (setq cont nil))
			 ((looking-at "\\s *\\($\\|%\\)")
			  (forward-line 1))
			 ((looking-at "\\s *,")
			  (incf res)
			  (goto-char (match-end 0)))
			 (t
			  (when (zerop res)
			    (incf res))
			  (forward-sexp 1))))
		 res)
	     (error nil))))))

;;;; Backend code checking

(add-hook 'erl-nodeup-hook 'erl-check-backend)

(defun erl-check-backend (node _fsm)
  "Check if we have the 'distel' module available on `node'.
If not then try to send the module over as a binary and load it in."
  (unless t ; tdistel-inhibit-backend-check
    (erl-spawn
      (erl-send `[rex ,node]
		`[,erl-self [call
			     code ensure_loaded (distel)
			     ,(erl-group-leader)]])
      (erl-receive (node)
	  ((['rex ['error _]]
	    (&erl-load-backend node))
	   (_ t))))))

(defun &erl-load-backend (node)
  (let* ((elisp-directory
	  (file-name-directory (or (locate-library "distel") load-file-name)))
	 (ebin-directory (concat elisp-directory "../ebin"))
	 (modules '()))
    (dolist (file (directory-files ebin-directory))
      (when (string-match "^\\(.*\\)\\.beam$" file)
	(let ((module (intern (match-string 1 file)))
	      (filename (concat ebin-directory "/" file)))
	  (push (list module filename) modules))))
    (if (null modules)
	(erl-warn-backend-problem "don't have beam files")
      (&erl-load-backend-modules node modules))))

(defun &erl-load-backend-modules (node modules)
  (message "loading = %S" (car modules))
  (if (null modules)
      (message "(Successfully uploaded backend modules into node)")
    (let* ((module (caar modules))
	   (filename (cadar modules))
	   (content (erl-file-to-string filename))
	   (binary (erl-binary content)))
      (erl-send `[rex ,node]
		`[,erl-self [call
			     code load_binary ,(list module filename binary)
			     ,(erl-group-leader)]])
      (erl-receive (node modules)
	  ((['rex ['error reason]]
	    (erl-warn-backend-problem reason))
	   (['rex _]
	    (&erl-load-backend-modules node (rest modules))))))))

(defun erl-warn-backend-problem (reason)
  (with-current-buffer (get-buffer-create "*Distel Warning*")
    (erase-buffer)
    (insert (format "\
Distel Warning: node `%s' can't seem to load the `distel' module.

This means that most Distel commands won't function correctly, because
the supporting library is not available. Please check your node's code
path, and make sure that Distel's \"ebin\" directory is included.

The most likely cause of this problem is either:

  a) Your ~/.erlang file doesn't add Distel to your load path (the
     Distel \"make config_install\" target can set this up for you.)

  b) Your system's boot script doesn't consult your ~/.erlang file to
     read your code path setting.

To disable this warning in future, set `distel-inhibit-backend-check' to t.

"
		    node))
    (display-buffer (current-buffer))
    (error "Unable to load or upload distel backend: %S" reason)))

(defun erl-file-to-string (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

;;;; RPC

(defun erl-rpc (k kargs node m f a)
  "Call {M,F,A} on NODE and deliver the result to the function K.
The first argument to K is the result from the RPC, followed by the
elements of KARGS."
  (erl-spawn
    (erl-send-rpc node m f a)
    (erl-rpc-receive k kargs)))

(defun erl-send-rpc (node mod fun args)
  "Send an RPC request on NODE to apply(MOD, FUN, ARGS).
The reply will be sent back as an asynchronous message of the form:
    [rex Result]
On an error, Result will be [badrpc Reason]."
  (let ((m 'wdistel)
	(f 'rpc_entry)
	(a (list mod fun args)))
    (erl-send (tuple 'rex node)
	      ;; {Who, {call, M, F, A, GroupLeader}}
	      (tuple erl-self (tuple 'call m f a (erl-group-leader))))))

(defun erl-rpc-receive (k kargs)
  "Receive the reply to an `erl-rpc'."
  (erl-receive (k kargs)
      ((['rex reply] (apply k (cons reply kargs))))))

(defun erpc (node m f a)
  "Make an RPC to an erlang node."
  (interactive (list (erl-target-node)
		     (intern (read-string "Module: "))
		     (intern (read-string "Function: "))
		     (eval-minibuffer "Args: ")))
  (erl-rpc (lambda (result) (message "RPC result: %S" result))
	   nil
	   node
	   m f a))

(defun erl-ping (node)
  "Ping the NODE, uploading distel code as a side effect."
  (interactive (list (erl-target-node)))
  (erl-spawn
    (erl-send-rpc node 'erlang 'node nil)
    (erl-receive (node)
	((['rex response]
          (if (equal node response)
              (message "Successfully communicated with remote node %S"
                       node)
            (message "Failed to communicate with node %S: %S"
                     node response)))))))

