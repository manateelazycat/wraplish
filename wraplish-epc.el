;;; epcs.el --- EPC Server              -*- lexical-binding: t -*-

;; Copyright (C) 2011,2012,2013  Masashi Sakurai

;; Author: Masashi Sakurai <m.sakurai at kiwanami.net>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; deferred
(cl-defmacro wraplish-deferred-chain (&rest elements)
  "Anaphoric function chain macro for deferred chains."
  (declare (debug (&rest form))
           (indent 0))
  `(let (it)
     ,@(cl-loop for i in elements
                collect
                `(setq it ,i))
     it))

;; Debug
(defvar wraplish-deferred-debug nil
  "Debug output switch.")

(defvar wraplish-deferred-debug-count 0
  "[internal] Debug output counter.")

(defun wraplish-deferred-log (&rest args)
  "[internal] Debug log function."
  (when wraplish-deferred-debug
    (with-current-buffer (get-buffer-create "*wraplish-deferred-log*")
      (save-excursion
        (goto-char (point-max))
        (insert (format "%5i %s\n\n\n" wraplish-deferred-debug-count (apply #'format args)))))
    (cl-incf wraplish-deferred-debug-count)))

(defvar wraplish-deferred-debug-on-signal nil
  "If non nil, the value `debug-on-signal' is substituted this
value in the `condition-case' form in deferred
implementations. Then, Emacs debugger can catch an error occurred
in the asynchronous tasks.")

(cl-defmacro wraplish-deferred-condition-case (var protected-form &rest handlers)
  "[internal] Custom condition-case. See the comment for
`wraplish-deferred-debug-on-signal'."
  (declare (debug condition-case)
           (indent 1))
  `(let ((debug-on-signal
          (or debug-on-signal wraplish-deferred-debug-on-signal)))
     (condition-case ,var
         ,protected-form
       ,@handlers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Back end functions of deferred tasks

(defvar wraplish-deferred-tick-time 0.001
  "Waiting time between asynchronous tasks (second).
The shorter waiting time increases the load of Emacs. The end
user can tune this parameter. However, applications should not
modify it because the applications run on various environments.")

(defvar wraplish-deferred-queue nil
  "[internal] The execution queue of deferred objects.
See the functions `wraplish-deferred-post-task' and `wraplish-deferred-worker'.")

(defun wraplish-deferred-post-task (d which &optional arg)
  "[internal] Add a deferred object to the execution queue
`wraplish-deferred-queue' and schedule to execute.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (let ((pack `(,d ,which . ,arg)))
    (push pack wraplish-deferred-queue)
    (wraplish-deferred-log "QUEUE-POST [%s]: %s" (length wraplish-deferred-queue) pack)
    (run-at-time wraplish-deferred-tick-time nil 'wraplish-deferred-worker)
    d))

(defun wraplish-deferred-worker ()
  "[internal] Consume a deferred task.
Mainly this function is called by timer asynchronously."
  (when wraplish-deferred-queue
    (let* ((pack (car (last wraplish-deferred-queue)))
           (d (car pack))
           (which (cadr pack))
           (arg (cddr pack)) value)
      (setq wraplish-deferred-queue (nbutlast wraplish-deferred-queue))
      (condition-case err
          (setq value (wraplish-deferred-exec-task d which arg))
        (error
         (wraplish-deferred-log "ERROR : %s" err)
         (message "deferred error : %s" err)))
      value)))

;; Struct: wraplish-deferred-object
;;
;; callback    : a callback function (default `identity')
;; errorback   : an errorback function (default `wraplish-deferred-resignal')
;; cancel      : a canceling function (default `wraplish-deferred-default-cancel')
;; next        : a next chained deferred object (default nil)
;; status      : if 'ok or 'ng, this deferred has a result (error) value. (default nil)
;; value       : saved value (default nil)
;;
(cl-defstruct wraplish-deferred-object
  (callback 'identity)
  (errorback 'wraplish-deferred-resignal)
  (cancel 'wraplish-deferred-default-cancel)
  next status value)

(defun wraplish-deferred-resignal (err)
  "[internal] Safely resignal ERR as an Emacs condition.

If ERR is a cons (ERROR-SYMBOL . DATA) where ERROR-SYMBOL has an
`error-conditions' property, it is re-signaled unchanged. If ERR
is a string, it is signaled as a generic error using `error'.
Otherwise, ERR is formatted into a string as if by `print' before
raising with `error'."
  (cond ((and (listp err)
              (symbolp (car err))
              (get (car err) 'error-conditions))
         (signal (car err) (cdr err)))
        ((stringp err)
         (error "%s" err))
        (t
         (error "%S" err))))

(defun wraplish-deferred-default-cancel (d)
  "[internal] Default canceling function."
  (wraplish-deferred-log "CANCEL : %s" d)
  (setf (wraplish-deferred-object-callback d) 'identity)
  (setf (wraplish-deferred-object-errorback d) 'wraplish-deferred-resignal)
  (setf (wraplish-deferred-object-next d) nil)
  d)

(defun wraplish-deferred-exec-task (d which &optional arg)
  "[internal] Executing deferred task. If the deferred object has
next deferred task or the return value is a deferred object, this
function adds the task to the execution queue.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (wraplish-deferred-log "EXEC : %s / %s / %s" d which arg)
  (when (null d) (error "wraplish-deferred-exec-task was given a nil."))
  (let ((callback (if (eq which 'ok)
                      (wraplish-deferred-object-callback d)
                    (wraplish-deferred-object-errorback d)))
        (next-deferred (wraplish-deferred-object-next d)))
    (cond
     (callback
      (wraplish-deferred-condition-case err
                                         (let ((value (funcall callback arg)))
                                           (cond
                                            ((wraplish-deferred-object-p value)
                                             (wraplish-deferred-log "WAIT NEST : %s" value)
                                             (if next-deferred
                                                 (wraplish-deferred-set-next value next-deferred)
                                               value))
                                            (t
                                             (if next-deferred
                                                 (wraplish-deferred-post-task next-deferred 'ok value)
                                               (setf (wraplish-deferred-object-status d) 'ok)
                                               (setf (wraplish-deferred-object-value d) value)
                                               value))))
                                         (error
                                          (cond
                                           (next-deferred
                                            (wraplish-deferred-post-task next-deferred 'ng err))
                                           (t
                                            (wraplish-deferred-log "ERROR : %S" err)
                                            (message "deferred error : %S" err)
                                            (setf (wraplish-deferred-object-status d) 'ng)
                                            (setf (wraplish-deferred-object-value d) err)
                                            err)))))
     (t                                 ; <= (null callback)
      (cond
       (next-deferred
        (wraplish-deferred-exec-task next-deferred which arg))
       ((eq which 'ok) arg)
       (t                               ; (eq which 'ng)
        (wraplish-deferred-resignal arg)))))))

(defun wraplish-deferred-set-next (prev next)
  "[internal] Connect deferred objects."
  (setf (wraplish-deferred-object-next prev) next)
  (cond
   ((eq 'ok (wraplish-deferred-object-status prev))
    (setf (wraplish-deferred-object-status prev) nil)
    (let ((ret (wraplish-deferred-exec-task
                next 'ok (wraplish-deferred-object-value prev))))
      (if (wraplish-deferred-object-p ret) ret
        next)))
   ((eq 'ng (wraplish-deferred-object-status prev))
    (setf (wraplish-deferred-object-status prev) nil)
    (let ((ret (wraplish-deferred-exec-task next 'ng (wraplish-deferred-object-value prev))))
      (if (wraplish-deferred-object-p ret) ret
        next)))
   (t
    next)))

(defun wraplish-deferred-new (&optional callback)
  "Create a deferred object."
  (if callback
      (make-wraplish-deferred-object :callback callback)
    (make-wraplish-deferred-object)))

(defun wraplish-deferred-callback (d &optional arg)
  "Start deferred chain with a callback message."
  (wraplish-deferred-exec-task d 'ok arg))

(defun wraplish-deferred-errorback (d &optional arg)
  "Start deferred chain with an errorback message."
  (declare (indent 1))
  (wraplish-deferred-exec-task d 'ng arg))

(defun wraplish-deferred-callback-post (d &optional arg)
  "Add the deferred object to the execution queue."
  (declare (indent 1))
  (wraplish-deferred-post-task d 'ok arg))

(defun wraplish-deferred-next (&optional callback arg)
  "Create a deferred object and schedule executing. This function
is a short cut of following code:
 (wraplish-deferred-callback-post (wraplish-deferred-new callback))."
  (let ((d (if callback
               (make-wraplish-deferred-object :callback callback)
             (make-wraplish-deferred-object))))
    (wraplish-deferred-callback-post d arg)
    d))

(defun wraplish-deferred-nextc (d callback)
  "Create a deferred object with OK callback and connect it to the given deferred object."
  (declare (indent 1))
  (let ((nd (make-wraplish-deferred-object :callback callback)))
    (wraplish-deferred-set-next d nd)))

(defun wraplish-deferred-error (d callback)
  "Create a deferred object with errorback and connect it to the given deferred object."
  (declare (indent 1))
  (let ((nd (make-wraplish-deferred-object :errorback callback)))
    (wraplish-deferred-set-next d nd)))

(defvar wraplish-epc-debug nil)

(defun wraplish-epc-log (&rest args)
  (when wraplish-epc-debug
    (with-current-buffer (get-buffer-create "*wraplish-epc-log*")
      (buffer-disable-undo)
      (goto-char (point-max))
      (insert (apply 'format args) "\n\n\n"))))

(defun wraplish-epc-make-procbuf (name)
  "[internal] Make a process buffer."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (set (make-local-variable 'kill-buffer-query-functions) nil)
      (erase-buffer) (buffer-disable-undo))
    buf))

(defvar wraplish-epc-uid 1)

(defun wraplish-epc-uid ()
  (cl-incf wraplish-epc-uid))

(defvar wraplish-epc-accept-process-timeout 150
  "Asynchronous timeout time. (msec)")

(put 'epc-error 'error-conditions '(error epc-error))
(put 'epc-error 'error-message "EPC Error")

(cl-defstruct wraplish-epc-connection
  "Set of information for network connection and event handling.

name    : Connection name. This name is used for process and buffer names.
process : Connection process object.
buffer  : Working buffer for the incoming data.
channel : Event channels for incoming messages."
  name process buffer channel)

(defun wraplish-epc-connect (host port)
  "[internal] Connect the server, initialize the process and
return wraplish-epc-connection object."
  (wraplish-epc-log ">> Connection start: %s:%s" host port)
  (let* ((connection-id (wraplish-epc-uid))
         (connection-name (format "wraplish-epc con %s" connection-id))
         (connection-buf (wraplish-epc-make-procbuf (format "*%s*" connection-name)))
         (connection-process
          (open-network-stream connection-name connection-buf host port))
         (channel (list connection-name nil))
         (connection (make-wraplish-epc-connection
                      :name connection-name
                      :process connection-process
                      :buffer connection-buf
                      :channel channel)))
    (wraplish-epc-log ">> Connection establish")
    (set-process-coding-system  connection-process 'binary 'binary)
    (set-process-filter connection-process
                        (lambda (p m)
                          (wraplish-epc-process-filter connection p m)))
    (set-process-sentinel connection-process
                          (lambda (p e)
                            (wraplish-epc-process-sentinel connection p e)))
    (set-process-query-on-exit-flag connection-process nil)
    connection))

(defun wraplish-epc-process-sentinel (connection process msg)
  (wraplish-epc-log "!! Process Sentinel [%s] : %S : %S"
                     (wraplish-epc-connection-name connection) process msg)
  (wraplish-epc-disconnect connection))

(defun wraplish-epc-net-send (connection sexp)
  (let* ((msg (encode-coding-string
               (concat (wraplish-epc-prin1-to-string sexp) "\n") 'utf-8-unix))
         (string (concat (format "%06x" (length msg)) msg))
         (proc (wraplish-epc-connection-process connection)))
    (wraplish-epc-log ">> SEND : [%S]" string)
    (process-send-string proc string)))

(defun wraplish-epc-disconnect (connection)
  (let ((process (wraplish-epc-connection-process connection))
        (buf (wraplish-epc-connection-buffer connection))
        (name (wraplish-epc-connection-name connection)))
    (wraplish-epc-log "!! Disconnect [%s]" name)
    (when process
      (set-process-sentinel process nil)
      (delete-process process)
      (when (get-buffer buf) (kill-buffer buf)))
    (wraplish-epc-log "!! Disconnected finished [%s]" name)))

(defun wraplish-epc-process-filter (connection process message)
  (wraplish-epc-log "INCOMING: [%s] [%S]" (wraplish-epc-connection-name connection) message)
  (with-current-buffer (wraplish-epc-connection-buffer connection)
    (goto-char (point-max))
    (insert message)
    (wraplish-epc-process-available-input connection process)))

(defun wraplish-epc-signal-connect (channel event-sym &optional callback)
  "Append an observer for EVENT-SYM of CHANNEL and return a deferred object.
If EVENT-SYM is `t', the observer receives all signals of the channel.
If CALLBACK function is given, the deferred object executes the
CALLBACK function asynchronously. One can connect subsequent
tasks to the returned deferred object."
  (let ((d (if callback
               (wraplish-deferred-new callback)
             (wraplish-deferred-new))))
    (push (cons event-sym d)
          (cddr channel))
    d))

(defun wraplish-epc-signal-send (channel event-sym &rest args)
  "Send a signal to CHANNEL. If ARGS values are given,
observers can get the values by following code:

  (lambda (event)
    (destructuring-bind
     (event-sym (args))
     event ... ))
"
  (let ((observers (cddr channel))
        (event (list event-sym args)))
    (cl-loop for i in observers
             for name = (car i)
             for d = (cdr i)
             if (or (eq event-sym name) (eq t name))
             do (wraplish-deferred-callback-post d event))))

(defun wraplish-epc-process-available-input (connection process)
  "Process all complete messages that have arrived from Lisp."
  (with-current-buffer (process-buffer process)
    (while (wraplish-epc-net-have-input-p)
      (let ((event (wraplish-epc-net-read-or-lose process))
            (ok nil))
        (wraplish-epc-log "<< RECV [%S]" event)
        (unwind-protect
            (condition-case err
                (progn
                  (apply 'wraplish-epc-signal-send
                         (cons (wraplish-epc-connection-channel connection) event))
                  (setq ok t))
              ('error (wraplish-epc-log "MsgError: %S / <= %S" err event)))
          (unless ok
            (wraplish-epc-process-available-input connection process)))))))

(defun wraplish-epc-net-have-input-p ()
  "Return true if a complete message is available."
  (goto-char (point-min))
  (and (>= (buffer-size) 6)
       (>= (- (buffer-size) 6) (wraplish-epc-net-decode-length))))

(defun wraplish-epc-net-read-or-lose (_process)
  (condition-case error
      (wraplish-epc-net-read)
    (error
     (debug 'error error)
     (error "net-read error: %S" error))))

(defun wraplish-epc-net-read ()
  "Read a message from the network buffer."
  (goto-char (point-min))
  (let* ((length (wraplish-epc-net-decode-length))
         (start (+ 6 (point)))
         (end (+ start length))
         _content)
    (cl-assert (cl-plusp length))
    (prog1 (save-restriction
             (narrow-to-region start end)
             (read (decode-coding-string
                    (buffer-string) 'utf-8-unix)))
      (delete-region (point-min) end))))

(defun wraplish-epc-net-decode-length ()
  "Read a 24-bit hex-encoded integer from buffer."
  (string-to-number (buffer-substring-no-properties (point) (+ (point) 6)) 16))

(defun wraplish-epc-prin1-to-string (sexp)
  "Like `prin1-to-string' but don't octal-escape non-ascii characters.
This is more compatible with the CL reader."
  (with-temp-buffer
    (let (print-escape-nonascii
          print-escape-newlines
          print-length
          print-level)
      (prin1 sexp (current-buffer))
      (buffer-string))))

(cl-defstruct wraplish-epc-manager
  "Root object that holds all information related to an EPC activity.

`wraplish-epc-start-epc' returns this object.

title          : instance name for displaying on the `wraplish-epc-controller' UI
server-process : process object for the peer
commands       : a list of (prog . args)
port           : port number
connection     : wraplish-epc-connection instance
methods        : alist of method (name . function)
sessions       : alist of session (id . deferred)
exit-hook      : functions for after shutdown EPC connection"
  title server-process commands port connection methods sessions exit-hooks)

(cl-defstruct wraplish-epc-method
  "Object to hold serving method information.

name       : method name (symbol)   ex: 'test
task       : method function (function with one argument)
arg-specs  : arg-specs (one string) ex: \"(A B C D)\"
docstring  : docstring (one string) ex: \"A test function. Return sum of A,B,C and D\"
"
  name task docstring arg-specs)

(defvar wraplish-epc-live-connections nil
  "[internal] A list of `wraplish-epc-manager' objects.
those objects currently connect to the epc peer.
This variable is for debug purpose.")

(defun wraplish-epc-server-process-name (uid)
  (format "wraplish-epc-server:%s" uid))

(defun wraplish-epc-server-buffer-name (uid)
  (format " *%s*" (wraplish-epc-server-process-name uid)))

(defun wraplish-epc-stop-epc (mngr)
  "Disconnect the connection for the server."
  (let* ((proc (wraplish-epc-manager-server-process mngr))
         (buf (and proc (process-buffer proc))))
    (wraplish-epc-disconnect (wraplish-epc-manager-connection mngr))
    (when proc
      (accept-process-output proc 0 wraplish-epc-accept-process-timeout t))
    (when (and proc (equal 'run (process-status proc)))
      (kill-process proc))
    (when buf  (kill-buffer buf))
    (setq wraplish-epc-live-connections (delete mngr wraplish-epc-live-connections))
    ))

(defun wraplish-epc-args (args)
  "[internal] If ARGS is an atom, return it. If list, return the cadr of it."
  (cond
   ((atom args) args)
   (t (cadr args))))

(defun wraplish-epc-init-epc-layer (mngr)
  "[internal] Connect to the server program and return an wraplish-epc-connection instance."
  (let* ((mngr mngr)
         (conn (wraplish-epc-manager-connection mngr))
         (channel (wraplish-epc-connection-channel conn)))
    ;; dispatch incoming messages with the lexical scope
    (cl-loop for (method . body) in
             `((call
                . (lambda (args)
                    (wraplish-epc-log "SIG CALL: %S" args)
                    (apply 'wraplish-epc-handler-called-method ,mngr (wraplish-epc-args args))))
               (return
                . (lambda (args)
                    (wraplish-epc-log "SIG RET: %S" args)
                    (apply 'wraplish-epc-handler-return ,mngr (wraplish-epc-args args))))
               (return-error
                . (lambda (args)
                    (wraplish-epc-log "SIG RET-ERROR: %S" args)
                    (apply 'wraplish-epc-handler-return-error ,mngr (wraplish-epc-args args))))
               (epc-error
                . (lambda (args)
                    (wraplish-epc-log "SIG EPC-ERROR: %S" args)
                    (apply 'wraplish-epc-handler-epc-error ,mngr (wraplish-epc-args args))))
               (methods
                . (lambda (args)
                    (wraplish-epc-log "SIG METHODS: %S" args)
                    (wraplish-epc-handler-methods ,mngr (caadr args))))
               ) do
             (wraplish-epc-signal-connect channel method body))
    (push mngr wraplish-epc-live-connections)
    mngr))

(defun wraplish-epc-manager-send (mngr method &rest messages)
  "[internal] low-level message sending."
  (let* ((conn (wraplish-epc-manager-connection mngr)))
    (wraplish-epc-net-send conn (cons method messages))))

(defun wraplish-epc-manager-get-method (mngr method-name)
  "[internal] Return a method object. If not found, return nil."
  (cl-loop for i in (wraplish-epc-manager-methods mngr)
           if (eq method-name (wraplish-epc-method-name i))
           do (cl-return i)))

(defun wraplish-epc-handler-methods (mngr uid)
  "[internal] Return a list of information for registered methods."
  (let ((info
         (cl-loop for i in (wraplish-epc-manager-methods mngr)
                  collect
                  (list
                   (wraplish-epc-method-name i)
                   (or (wraplish-epc-method-arg-specs i) "")
                   (or (wraplish-epc-method-docstring i) "")))))
    (wraplish-epc-manager-send mngr 'return uid info)))

(defun wraplish-epc-handler-called-method (mngr uid name args)
  "[internal] low-level message handler for peer's calling."
  (let ((mngr mngr) (uid uid))
    (let* ((_methods (wraplish-epc-manager-methods mngr))
           (method (wraplish-epc-manager-get-method mngr name)))
      (cond
       ((null method)
        (wraplish-epc-log "ERR: No such method : %s" name)
        (wraplish-epc-manager-send mngr 'epc-error uid (format "EPC-ERROR: No such method : %s" name)))
       (t
        (condition-case err
            (let* ((f (wraplish-epc-method-task method))
                   (ret (apply f args)))
              (cond
               ((wraplish-deferred-object-p ret)
                (wraplish-deferred-nextc ret
                                          (lambda (xx) (wraplish-epc-manager-send mngr 'return uid xx))))
               (t (wraplish-epc-manager-send mngr 'return uid ret))))
          (error
           (wraplish-epc-log "ERROR : %S" err)
           (wraplish-epc-manager-send mngr 'return-error uid err))))))))

(defun wraplish-epc-manager-remove-session (mngr uid)
  "[internal] Remove a session from the epc manager object."
  (cl-loop with ret = nil
           for pair in (wraplish-epc-manager-sessions mngr)
           unless (eq uid (car pair))
           do (push pair ret)
           finally
           do (setf (wraplish-epc-manager-sessions mngr) ret)))

(defun wraplish-epc-handler-return (mngr uid args)
  "[internal] low-level message handler for normal returns."
  (let ((pair (assq uid (wraplish-epc-manager-sessions mngr))))
    (cond
     (pair
      (wraplish-epc-log "RET: id:%s [%S]" uid args)
      (wraplish-epc-manager-remove-session mngr uid)
      (wraplish-deferred-callback (cdr pair) args))
     (t                                 ; error
      (wraplish-epc-log "RET: NOT FOUND: id:%s [%S]" uid args)))))

(defun wraplish-epc-handler-return-error (mngr uid args)
  "[internal] low-level message handler for application errors."
  (let ((pair (assq uid (wraplish-epc-manager-sessions mngr))))
    (cond
     (pair
      (wraplish-epc-log "RET-ERR: id:%s [%S]" uid args)
      (wraplish-epc-manager-remove-session mngr uid)
      (wraplish-deferred-errorback (cdr pair) (format "%S" args)))
     (t                                 ; error
      (wraplish-epc-log "RET-ERR: NOT FOUND: id:%s [%S]" uid args)))))

(defun wraplish-epc-handler-epc-error (mngr uid args)
  "[internal] low-level message handler for epc errors."
  (let ((pair (assq uid (wraplish-epc-manager-sessions mngr))))
    (cond
     (pair
      (wraplish-epc-log "RET-EPC-ERR: id:%s [%S]" uid args)
      (wraplish-epc-manager-remove-session mngr uid)
      (wraplish-deferred-errorback (cdr pair) (list 'epc-error args)))
     (t                                 ; error
      (wraplish-epc-log "RET-EPC-ERR: NOT FOUND: id:%s [%S]" uid args)))))

(defun wraplish-epc-call-deferred (mngr method-name args)
  "Call peer's method with args asynchronously. Return a deferred
object which is called with the result."
  (let ((uid (wraplish-epc-uid))
        (sessions (wraplish-epc-manager-sessions mngr))
        (d (wraplish-deferred-new)))
    (push (cons uid d) sessions)
    (setf (wraplish-epc-manager-sessions mngr) sessions)
    (wraplish-epc-manager-send mngr 'call uid method-name args)
    d))

(defun wraplish-epc-define-method (mngr method-name task &optional arg-specs docstring)
  "Define a method and return a deferred object which is called by the peer."
  (let* ((method (make-wraplish-epc-method
                  :name method-name :task task
                  :arg-specs arg-specs :docstring docstring))
         (methods (cons method (wraplish-epc-manager-methods mngr))))
    (setf (wraplish-epc-manager-methods mngr) methods)
    method))

(defun wraplish-epc-sync (mngr d)
  "Wrap deferred methods with synchronous waiting, and return the result.
If an exception is occurred, this function throws the error."
  (let ((result 'wraplish-epc-nothing))
    (wraplish-deferred-chain
     d
     (wraplish-deferred-nextc it
                               (lambda (x) (setq result x)))
     (wraplish-deferred-error it
                               (lambda (er) (setq result (cons 'error er)))))
    (while (eq result 'wraplish-epc-nothing)
      (save-current-buffer
        (accept-process-output
         (wraplish-epc-connection-process (wraplish-epc-manager-connection mngr))
         0 wraplish-epc-accept-process-timeout t)))
    (if (and (consp result) (eq 'error (car result)))
        (error (cdr result)) result)))

(defun wraplish-epc-call-sync (mngr method-name args)
  "Call peer's method with args synchronously and return the result.
If an exception is occurred, this function throws the error."
  (wraplish-epc-sync mngr (wraplish-epc-call-deferred mngr method-name args)))

(defun wraplish-epc-live-p (mngr)
  "Return non-nil when MNGR is an EPC manager object with a live
connection."
  (let ((proc (ignore-errors
                (wraplish-epc-connection-process (wraplish-epc-manager-connection mngr)))))
    (and (processp proc)
         ;; Same as `process-live-p' in Emacs >= 24:
         (memq (process-status proc) '(run open listen connect stop)))))

;; epcs
(defvar wraplish-epc-server-client-processes nil
  "[internal] A list of ([process object] . [`wraplish-epc-manager' instance]).
When the server process accepts the client connection, the
`wraplish-epc-manager' instance is created and stored in this variable
`wraplish-epc-server-client-processes'. This variable is used for the management
purpose.")

;; wraplish-epc-server
;;   name    : process name (string)   ex: "EPC Server 1"
;;   process : server process object
;;   port    : port number
;;   connect-function : initialize function for `wraplish-epc-manager' instances
(cl-defstruct wraplish-epc-server name process port connect-function)

(defvar wraplish-epc-server-processes nil
  "[internal] A list of ([process object] . [`wraplish-epc-server' instance]).
This variable is used for the management purpose.")

(defun wraplish-epc-server-get-manager-by-process (proc)
  "[internal] Return the wraplish-epc-manager instance for the PROC."
  (cl-loop for (pp . mngr) in wraplish-epc-server-client-processes
           if (eql pp proc)
           do (cl-return mngr)
           finally return nil))

(defun wraplish-epc-server-accept (process)
  "[internal] Initialize the process and return wraplish-epc-manager object."
  (wraplish-epc-log "LSPBRIDGE-EPC-SERVER- >> Connection accept: %S" process)
  (let* ((connection-id (wraplish-epc-uid))
         (connection-name (format "wraplish-epc con %s" connection-id))
         (channel (list connection-name nil))
         (connection (make-wraplish-epc-connection
                      :name connection-name
                      :process process
                      :buffer (process-buffer process)
                      :channel channel)))
    (wraplish-epc-log "LSPBRIDGE-EPC-SERVER- >> Connection establish")
    (set-process-coding-system process 'binary 'binary)
    (set-process-filter process
                        (lambda (p m)
                          (wraplish-epc-process-filter connection p m)))
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel process
                          (lambda (p e)
                            (wraplish-epc-process-sentinel connection p e)))
    (make-wraplish-epc-manager :server-process process :port t
                                :connection connection)))

(defun wraplish-epc-server-sentinel (process message connect-function)
  "[internal] Process sentinel handler for the server process."
  (wraplish-epc-log "LSPBRIDGE-EPC-SERVER- SENTINEL: %S %S" process message)
  (let ((mngr (wraplish-epc-server-get-manager-by-process process)))
    (cond
     ;; new connection
     ((and (string-match "open" message) (null mngr))
      (condition-case err
          (let ((mngr (wraplish-epc-server-accept process)))
            (push (cons process mngr) wraplish-epc-server-client-processes)
            (wraplish-epc-init-epc-layer mngr)
            (when connect-function (funcall connect-function mngr))
            mngr)
        ('error
         (wraplish-epc-log "LSPBRIDGE-EPC-SERVER- Protocol error: %S" err)
         (wraplish-epc-log "LSPBRIDGE-EPC-SERVER- ABORT %S" process)
         (delete-process process))))
     ;; ignore
     ((null mngr) nil )
     ;; disconnect
     (t
      (let ((pair (assq process wraplish-epc-server-client-processes)) _d)
        (when pair
          (wraplish-epc-log "LSPBRIDGE-EPC-SERVER- DISCONNECT %S" process)
          (wraplish-epc-stop-epc (cdr pair))
          (setq wraplish-epc-server-client-processes
                (assq-delete-all process wraplish-epc-server-client-processes))
          ))
      nil))))

(defun wraplish-epc-server-start (connect-function &optional port)
  "Start TCP Server and return the main process object."
  (let*
      ((connect-function connect-function)
       (name (format "WRAPLISH EPC Server %s" (wraplish-epc-uid)))
       (buf (wraplish-epc-make-procbuf (format " *%s*" name)))
       (main-process
        (make-network-process
         :name name
         :buffer buf
         :family 'ipv4
         :server t
         :host "127.0.0.1"
         :service (or port t)
         :noquery t
         :sentinel
         (lambda (process message)
           (wraplish-epc-server-sentinel process message connect-function)))))
    (push (cons main-process
                (make-wraplish-epc-server
                 :name name :process main-process
                 :port (process-contact main-process :service)
                 :connect-function connect-function))
          wraplish-epc-server-processes)
    main-process))

(provide 'wraplish-epc)
;;; wraplish-epc.el ends here
