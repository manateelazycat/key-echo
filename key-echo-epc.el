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
(cl-defmacro key-echo-deferred-chain (&rest elements)
  "Anaphoric function chain macro for deferred chains."
  (declare (debug (&rest form))
           (indent 0))
  `(let (it)
     ,@(cl-loop for i in elements
                collect
                `(setq it ,i))
     it))

;; Debug
(defvar key-echo-deferred-debug nil
  "Debug output switch.")

(defvar key-echo-deferred-debug-count 0
  "[internal] Debug output counter.")

(defun key-echo-deferred-log (&rest args)
  "[internal] Debug log function."
  (when key-echo-deferred-debug
    (with-current-buffer (get-buffer-create "*key-echo-deferred-log*")
      (save-excursion
        (goto-char (point-max))
        (insert (format "%5i %s\n\n\n" key-echo-deferred-debug-count (apply #'format args)))))
    (cl-incf key-echo-deferred-debug-count)))

(defvar key-echo-deferred-debug-on-signal nil
  "If non nil, the value `debug-on-signal' is substituted this
value in the `condition-case' form in deferred
implementations. Then, Emacs debugger can catch an error occurred
in the asynchronous tasks.")

(cl-defmacro key-echo-deferred-condition-case (var protected-form &rest handlers)
  "[internal] Custom condition-case. See the comment for
`key-echo-deferred-debug-on-signal'."
  (declare (debug condition-case)
           (indent 1))
  `(let ((debug-on-signal
          (or debug-on-signal key-echo-deferred-debug-on-signal)))
     (condition-case ,var
         ,protected-form
       ,@handlers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Back end functions of deferred tasks

(defvar key-echo-deferred-tick-time 0.001
  "Waiting time between asynchronous tasks (second).
The shorter waiting time increases the load of Emacs. The end
user can tune this parameter. However, applications should not
modify it because the applications run on various environments.")

(defvar key-echo-deferred-queue nil
  "[internal] The execution queue of deferred objects.
See the functions `key-echo-deferred-post-task' and `key-echo-deferred-worker'.")

(defun key-echo-deferred-post-task (d which &optional arg)
  "[internal] Add a deferred object to the execution queue
`key-echo-deferred-queue' and schedule to execute.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (let ((pack `(,d ,which . ,arg)))
    (push pack key-echo-deferred-queue)
    (key-echo-deferred-log "QUEUE-POST [%s]: %s" (length key-echo-deferred-queue) pack)
    (run-at-time key-echo-deferred-tick-time nil 'key-echo-deferred-worker)
    d))

(defun key-echo-deferred-worker ()
  "[internal] Consume a deferred task.
Mainly this function is called by timer asynchronously."
  (when key-echo-deferred-queue
    (let* ((pack (car (last key-echo-deferred-queue)))
           (d (car pack))
           (which (cadr pack))
           (arg (cddr pack)) value)
      (setq key-echo-deferred-queue (nbutlast key-echo-deferred-queue))
      (condition-case err
          (setq value (key-echo-deferred-exec-task d which arg))
        (error
         (key-echo-deferred-log "ERROR : %s" err)
         (message "deferred error : %s" err)))
      value)))

;; Struct: key-echo-deferred-object
;;
;; callback    : a callback function (default `identity')
;; errorback   : an errorback function (default `key-echo-deferred-resignal')
;; cancel      : a canceling function (default `key-echo-deferred-default-cancel')
;; next        : a next chained deferred object (default nil)
;; status      : if 'ok or 'ng, this deferred has a result (error) value. (default nil)
;; value       : saved value (default nil)
;;
(cl-defstruct key-echo-deferred-object
  (callback 'identity)
  (errorback 'key-echo-deferred-resignal)
  (cancel 'key-echo-deferred-default-cancel)
  next status value)

(defun key-echo-deferred-resignal (err)
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

(defun key-echo-deferred-default-cancel (d)
  "[internal] Default canceling function."
  (key-echo-deferred-log "CANCEL : %s" d)
  (setf (key-echo-deferred-object-callback d) 'identity)
  (setf (key-echo-deferred-object-errorback d) 'key-echo-deferred-resignal)
  (setf (key-echo-deferred-object-next d) nil)
  d)

(defun key-echo-deferred-exec-task (d which &optional arg)
  "[internal] Executing deferred task. If the deferred object has
next deferred task or the return value is a deferred object, this
function adds the task to the execution queue.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (key-echo-deferred-log "EXEC : %s / %s / %s" d which arg)
  (when (null d) (error "key-echo-deferred-exec-task was given a nil."))
  (let ((callback (if (eq which 'ok)
                      (key-echo-deferred-object-callback d)
                    (key-echo-deferred-object-errorback d)))
        (next-deferred (key-echo-deferred-object-next d)))
    (cond
     (callback
      (key-echo-deferred-condition-case err
                                         (let ((value (funcall callback arg)))
                                           (cond
                                            ((key-echo-deferred-object-p value)
                                             (key-echo-deferred-log "WAIT NEST : %s" value)
                                             (if next-deferred
                                                 (key-echo-deferred-set-next value next-deferred)
                                               value))
                                            (t
                                             (if next-deferred
                                                 (key-echo-deferred-post-task next-deferred 'ok value)
                                               (setf (key-echo-deferred-object-status d) 'ok)
                                               (setf (key-echo-deferred-object-value d) value)
                                               value))))
                                         (error
                                          (cond
                                           (next-deferred
                                            (key-echo-deferred-post-task next-deferred 'ng err))
                                           (t
                                            (key-echo-deferred-log "ERROR : %S" err)
                                            (message "deferred error : %S" err)
                                            (setf (key-echo-deferred-object-status d) 'ng)
                                            (setf (key-echo-deferred-object-value d) err)
                                            err)))))
     (t                                 ; <= (null callback)
      (cond
       (next-deferred
        (key-echo-deferred-exec-task next-deferred which arg))
       ((eq which 'ok) arg)
       (t                               ; (eq which 'ng)
        (key-echo-deferred-resignal arg)))))))

(defun key-echo-deferred-set-next (prev next)
  "[internal] Connect deferred objects."
  (setf (key-echo-deferred-object-next prev) next)
  (cond
   ((eq 'ok (key-echo-deferred-object-status prev))
    (setf (key-echo-deferred-object-status prev) nil)
    (let ((ret (key-echo-deferred-exec-task
                next 'ok (key-echo-deferred-object-value prev))))
      (if (key-echo-deferred-object-p ret) ret
        next)))
   ((eq 'ng (key-echo-deferred-object-status prev))
    (setf (key-echo-deferred-object-status prev) nil)
    (let ((ret (key-echo-deferred-exec-task next 'ng (key-echo-deferred-object-value prev))))
      (if (key-echo-deferred-object-p ret) ret
        next)))
   (t
    next)))

(defun key-echo-deferred-new (&optional callback)
  "Create a deferred object."
  (if callback
      (make-key-echo-deferred-object :callback callback)
    (make-key-echo-deferred-object)))

(defun key-echo-deferred-callback (d &optional arg)
  "Start deferred chain with a callback message."
  (key-echo-deferred-exec-task d 'ok arg))

(defun key-echo-deferred-errorback (d &optional arg)
  "Start deferred chain with an errorback message."
  (declare (indent 1))
  (key-echo-deferred-exec-task d 'ng arg))

(defun key-echo-deferred-callback-post (d &optional arg)
  "Add the deferred object to the execution queue."
  (declare (indent 1))
  (key-echo-deferred-post-task d 'ok arg))

(defun key-echo-deferred-next (&optional callback arg)
  "Create a deferred object and schedule executing. This function
is a short cut of following code:
 (key-echo-deferred-callback-post (key-echo-deferred-new callback))."
  (let ((d (if callback
               (make-key-echo-deferred-object :callback callback)
             (make-key-echo-deferred-object))))
    (key-echo-deferred-callback-post d arg)
    d))

(defun key-echo-deferred-nextc (d callback)
  "Create a deferred object with OK callback and connect it to the given deferred object."
  (declare (indent 1))
  (let ((nd (make-key-echo-deferred-object :callback callback)))
    (key-echo-deferred-set-next d nd)))

(defun key-echo-deferred-error (d callback)
  "Create a deferred object with errorback and connect it to the given deferred object."
  (declare (indent 1))
  (let ((nd (make-key-echo-deferred-object :errorback callback)))
    (key-echo-deferred-set-next d nd)))

(defvar key-echo-epc-debug nil)

(defun key-echo-epc-log (&rest args)
  (when key-echo-epc-debug
    (with-current-buffer (get-buffer-create "*key-echo-epc-log*")
      (buffer-disable-undo)
      (goto-char (point-max))
      (insert (apply 'format args) "\n\n\n"))))

(defun key-echo-epc-make-procbuf (name)
  "[internal] Make a process buffer."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (set (make-local-variable 'kill-buffer-query-functions) nil)
      (erase-buffer) (buffer-disable-undo))
    buf))

(defvar key-echo-epc-uid 1)

(defun key-echo-epc-uid ()
  (cl-incf key-echo-epc-uid))

(defvar key-echo-epc-accept-process-timeout 150
  "Asynchronous timeout time. (msec)")

(put 'epc-error 'error-conditions '(error epc-error))
(put 'epc-error 'error-message "EPC Error")

(cl-defstruct key-echo-epc-connection
  "Set of information for network connection and event handling.

name    : Connection name. This name is used for process and buffer names.
process : Connection process object.
buffer  : Working buffer for the incoming data.
channel : Event channels for incoming messages."
  name process buffer channel)

(defun key-echo-epc-connect (host port)
  "[internal] Connect the server, initialize the process and
return key-echo-epc-connection object."
  (key-echo-epc-log ">> Connection start: %s:%s" host port)
  (let* ((connection-id (key-echo-epc-uid))
         (connection-name (format "key-echo-epc con %s" connection-id))
         (connection-buf (key-echo-epc-make-procbuf (format "*%s*" connection-name)))
         (connection-process
          (open-network-stream connection-name connection-buf host port))
         (channel (list connection-name nil))
         (connection (make-key-echo-epc-connection
                      :name connection-name
                      :process connection-process
                      :buffer connection-buf
                      :channel channel)))
    (key-echo-epc-log ">> Connection establish")
    (set-process-coding-system  connection-process 'binary 'binary)
    (set-process-filter connection-process
                        (lambda (p m)
                          (key-echo-epc-process-filter connection p m)))
    (set-process-sentinel connection-process
                          (lambda (p e)
                            (key-echo-epc-process-sentinel connection p e)))
    (set-process-query-on-exit-flag connection-process nil)
    connection))

(defun key-echo-epc-process-sentinel (connection process msg)
  (key-echo-epc-log "!! Process Sentinel [%s] : %S : %S"
                     (key-echo-epc-connection-name connection) process msg)
  (key-echo-epc-disconnect connection))

(defun key-echo-epc-net-send (connection sexp)
  (let* ((msg (encode-coding-string
               (concat (key-echo-epc-prin1-to-string sexp) "\n") 'utf-8-unix))
         (string (concat (format "%06x" (length msg)) msg))
         (proc (key-echo-epc-connection-process connection)))
    (key-echo-epc-log ">> SEND : [%S]" string)
    (process-send-string proc string)))

(defun key-echo-epc-disconnect (connection)
  (let ((process (key-echo-epc-connection-process connection))
        (buf (key-echo-epc-connection-buffer connection))
        (name (key-echo-epc-connection-name connection)))
    (key-echo-epc-log "!! Disconnect [%s]" name)
    (when process
      (set-process-sentinel process nil)
      (delete-process process)
      (when (get-buffer buf) (kill-buffer buf)))
    (key-echo-epc-log "!! Disconnected finished [%s]" name)))

(defun key-echo-epc-process-filter (connection process message)
  (key-echo-epc-log "INCOMING: [%s] [%S]" (key-echo-epc-connection-name connection) message)
  (with-current-buffer (key-echo-epc-connection-buffer connection)
    (goto-char (point-max))
    (insert message)
    (key-echo-epc-process-available-input connection process)))

(defun key-echo-epc-signal-connect (channel event-sym &optional callback)
  "Append an observer for EVENT-SYM of CHANNEL and return a deferred object.
If EVENT-SYM is `t', the observer receives all signals of the channel.
If CALLBACK function is given, the deferred object executes the
CALLBACK function asynchronously. One can connect subsequent
tasks to the returned deferred object."
  (let ((d (if callback
               (key-echo-deferred-new callback)
             (key-echo-deferred-new))))
    (push (cons event-sym d)
          (cddr channel))
    d))

(defun key-echo-epc-signal-send (channel event-sym &rest args)
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
             do (key-echo-deferred-callback-post d event))))

(defun key-echo-epc-process-available-input (connection process)
  "Process all complete messages that have arrived from Lisp."
  (with-current-buffer (process-buffer process)
    (while (key-echo-epc-net-have-input-p)
      (let ((event (key-echo-epc-net-read-or-lose process))
            (ok nil))
        (key-echo-epc-log "<< RECV [%S]" event)
        (unwind-protect
            (condition-case err
                (progn
                  (apply 'key-echo-epc-signal-send
                         (cons (key-echo-epc-connection-channel connection) event))
                  (setq ok t))
              ('error (key-echo-epc-log "MsgError: %S / <= %S" err event)))
          (unless ok
            (key-echo-epc-process-available-input connection process)))))))

(defun key-echo-epc-net-have-input-p ()
  "Return true if a complete message is available."
  (goto-char (point-min))
  (and (>= (buffer-size) 6)
       (>= (- (buffer-size) 6) (key-echo-epc-net-decode-length))))

(defun key-echo-epc-net-read-or-lose (_process)
  (condition-case error
      (key-echo-epc-net-read)
    (error
     (debug 'error error)
     (error "net-read error: %S" error))))

(defun key-echo-epc-net-read ()
  "Read a message from the network buffer."
  (goto-char (point-min))
  (let* ((length (key-echo-epc-net-decode-length))
         (start (+ 6 (point)))
         (end (+ start length))
         _content)
    (cl-assert (cl-plusp length))
    (prog1 (save-restriction
             (narrow-to-region start end)
             (read (decode-coding-string
                    (buffer-string) 'utf-8-unix)))
      (delete-region (point-min) end))))

(defun key-echo-epc-net-decode-length ()
  "Read a 24-bit hex-encoded integer from buffer."
  (string-to-number (buffer-substring-no-properties (point) (+ (point) 6)) 16))

(defun key-echo-epc-prin1-to-string (sexp)
  "Like `prin1-to-string' but don't octal-escape non-ascii characters.
This is more compatible with the CL reader."
  (with-temp-buffer
    (let (print-escape-nonascii
          print-escape-newlines
          print-length
          print-level)
      (prin1 sexp (current-buffer))
      (buffer-string))))

(cl-defstruct key-echo-epc-manager
  "Root object that holds all information related to an EPC activity.

`key-echo-epc-start-epc' returns this object.

title          : instance name for displaying on the `key-echo-epc-controller' UI
server-process : process object for the peer
commands       : a list of (prog . args)
port           : port number
connection     : key-echo-epc-connection instance
methods        : alist of method (name . function)
sessions       : alist of session (id . deferred)
exit-hook      : functions for after shutdown EPC connection"
  title server-process commands port connection methods sessions exit-hooks)

(cl-defstruct key-echo-epc-method
  "Object to hold serving method information.

name       : method name (symbol)   ex: 'test
task       : method function (function with one argument)
arg-specs  : arg-specs (one string) ex: \"(A B C D)\"
docstring  : docstring (one string) ex: \"A test function. Return sum of A,B,C and D\"
"
  name task docstring arg-specs)

(defvar key-echo-epc-live-connections nil
  "[internal] A list of `key-echo-epc-manager' objects.
those objects currently connect to the epc peer.
This variable is for debug purpose.")

(defun key-echo-epc-server-process-name (uid)
  (format "key-echo-epc-server:%s" uid))

(defun key-echo-epc-server-buffer-name (uid)
  (format " *%s*" (key-echo-epc-server-process-name uid)))

(defun key-echo-epc-stop-epc (mngr)
  "Disconnect the connection for the server."
  (let* ((proc (key-echo-epc-manager-server-process mngr))
         (buf (and proc (process-buffer proc))))
    (key-echo-epc-disconnect (key-echo-epc-manager-connection mngr))
    (when proc
      (accept-process-output proc 0 key-echo-epc-accept-process-timeout t))
    (when (and proc (equal 'run (process-status proc)))
      (kill-process proc))
    (when buf  (kill-buffer buf))
    (setq key-echo-epc-live-connections (delete mngr key-echo-epc-live-connections))
    ))

(defun key-echo-epc-args (args)
  "[internal] If ARGS is an atom, return it. If list, return the cadr of it."
  (cond
   ((atom args) args)
   (t (cadr args))))

(defun key-echo-epc-init-epc-layer (mngr)
  "[internal] Connect to the server program and return an key-echo-epc-connection instance."
  (let* ((mngr mngr)
         (conn (key-echo-epc-manager-connection mngr))
         (channel (key-echo-epc-connection-channel conn)))
    ;; dispatch incoming messages with the lexical scope
    (cl-loop for (method . body) in
             `((call
                . (lambda (args)
                    (key-echo-epc-log "SIG CALL: %S" args)
                    (apply 'key-echo-epc-handler-called-method ,mngr (key-echo-epc-args args))))
               (return
                . (lambda (args)
                    (key-echo-epc-log "SIG RET: %S" args)
                    (apply 'key-echo-epc-handler-return ,mngr (key-echo-epc-args args))))
               (return-error
                . (lambda (args)
                    (key-echo-epc-log "SIG RET-ERROR: %S" args)
                    (apply 'key-echo-epc-handler-return-error ,mngr (key-echo-epc-args args))))
               (epc-error
                . (lambda (args)
                    (key-echo-epc-log "SIG EPC-ERROR: %S" args)
                    (apply 'key-echo-epc-handler-epc-error ,mngr (key-echo-epc-args args))))
               (methods
                . (lambda (args)
                    (key-echo-epc-log "SIG METHODS: %S" args)
                    (key-echo-epc-handler-methods ,mngr (caadr args))))
               ) do
             (key-echo-epc-signal-connect channel method body))
    (push mngr key-echo-epc-live-connections)
    mngr))

(defun key-echo-epc-manager-send (mngr method &rest messages)
  "[internal] low-level message sending."
  (let* ((conn (key-echo-epc-manager-connection mngr)))
    (key-echo-epc-net-send conn (cons method messages))))

(defun key-echo-epc-manager-get-method (mngr method-name)
  "[internal] Return a method object. If not found, return nil."
  (cl-loop for i in (key-echo-epc-manager-methods mngr)
           if (eq method-name (key-echo-epc-method-name i))
           do (cl-return i)))

(defun key-echo-epc-handler-methods (mngr uid)
  "[internal] Return a list of information for registered methods."
  (let ((info
         (cl-loop for i in (key-echo-epc-manager-methods mngr)
                  collect
                  (list
                   (key-echo-epc-method-name i)
                   (or (key-echo-epc-method-arg-specs i) "")
                   (or (key-echo-epc-method-docstring i) "")))))
    (key-echo-epc-manager-send mngr 'return uid info)))

(defun key-echo-epc-handler-called-method (mngr uid name args)
  "[internal] low-level message handler for peer's calling."
  (let ((mngr mngr) (uid uid))
    (let* ((_methods (key-echo-epc-manager-methods mngr))
           (method (key-echo-epc-manager-get-method mngr name)))
      (cond
       ((null method)
        (key-echo-epc-log "ERR: No such method : %s" name)
        (key-echo-epc-manager-send mngr 'epc-error uid (format "EPC-ERROR: No such method : %s" name)))
       (t
        (condition-case err
            (let* ((f (key-echo-epc-method-task method))
                   (ret (apply f args)))
              (cond
               ((key-echo-deferred-object-p ret)
                (key-echo-deferred-nextc ret
                                          (lambda (xx) (key-echo-epc-manager-send mngr 'return uid xx))))
               (t (key-echo-epc-manager-send mngr 'return uid ret))))
          (error
           (key-echo-epc-log "ERROR : %S" err)
           (key-echo-epc-manager-send mngr 'return-error uid err))))))))

(defun key-echo-epc-manager-remove-session (mngr uid)
  "[internal] Remove a session from the epc manager object."
  (cl-loop with ret = nil
           for pair in (key-echo-epc-manager-sessions mngr)
           unless (eq uid (car pair))
           do (push pair ret)
           finally
           do (setf (key-echo-epc-manager-sessions mngr) ret)))

(defun key-echo-epc-handler-return (mngr uid args)
  "[internal] low-level message handler for normal returns."
  (let ((pair (assq uid (key-echo-epc-manager-sessions mngr))))
    (cond
     (pair
      (key-echo-epc-log "RET: id:%s [%S]" uid args)
      (key-echo-epc-manager-remove-session mngr uid)
      (key-echo-deferred-callback (cdr pair) args))
     (t                                 ; error
      (key-echo-epc-log "RET: NOT FOUND: id:%s [%S]" uid args)))))

(defun key-echo-epc-handler-return-error (mngr uid args)
  "[internal] low-level message handler for application errors."
  (let ((pair (assq uid (key-echo-epc-manager-sessions mngr))))
    (cond
     (pair
      (key-echo-epc-log "RET-ERR: id:%s [%S]" uid args)
      (key-echo-epc-manager-remove-session mngr uid)
      (key-echo-deferred-errorback (cdr pair) (format "%S" args)))
     (t                                 ; error
      (key-echo-epc-log "RET-ERR: NOT FOUND: id:%s [%S]" uid args)))))

(defun key-echo-epc-handler-epc-error (mngr uid args)
  "[internal] low-level message handler for epc errors."
  (let ((pair (assq uid (key-echo-epc-manager-sessions mngr))))
    (cond
     (pair
      (key-echo-epc-log "RET-EPC-ERR: id:%s [%S]" uid args)
      (key-echo-epc-manager-remove-session mngr uid)
      (key-echo-deferred-errorback (cdr pair) (list 'epc-error args)))
     (t                                 ; error
      (key-echo-epc-log "RET-EPC-ERR: NOT FOUND: id:%s [%S]" uid args)))))

(defun key-echo-epc-call-deferred (mngr method-name args)
  "Call peer's method with args asynchronously. Return a deferred
object which is called with the result."
  (let ((uid (key-echo-epc-uid))
        (sessions (key-echo-epc-manager-sessions mngr))
        (d (key-echo-deferred-new)))
    (push (cons uid d) sessions)
    (setf (key-echo-epc-manager-sessions mngr) sessions)
    (key-echo-epc-manager-send mngr 'call uid method-name args)
    d))

(defun key-echo-epc-define-method (mngr method-name task &optional arg-specs docstring)
  "Define a method and return a deferred object which is called by the peer."
  (let* ((method (make-key-echo-epc-method
                  :name method-name :task task
                  :arg-specs arg-specs :docstring docstring))
         (methods (cons method (key-echo-epc-manager-methods mngr))))
    (setf (key-echo-epc-manager-methods mngr) methods)
    method))

(defun key-echo-epc-sync (mngr d)
  "Wrap deferred methods with synchronous waiting, and return the result.
If an exception is occurred, this function throws the error."
  (let ((result 'key-echo-epc-nothing))
    (key-echo-deferred-chain
     d
     (key-echo-deferred-nextc it
                               (lambda (x) (setq result x)))
     (key-echo-deferred-error it
                               (lambda (er) (setq result (cons 'error er)))))
    (while (eq result 'key-echo-epc-nothing)
      (save-current-buffer
        (accept-process-output
         (key-echo-epc-connection-process (key-echo-epc-manager-connection mngr))
         0 key-echo-epc-accept-process-timeout t)))
    (if (and (consp result) (eq 'error (car result)))
        (error (cdr result)) result)))

(defun key-echo-epc-call-sync (mngr method-name args)
  "Call peer's method with args synchronously and return the result.
If an exception is occurred, this function throws the error."
  (key-echo-epc-sync mngr (key-echo-epc-call-deferred mngr method-name args)))

(defun key-echo-epc-live-p (mngr)
  "Return non-nil when MNGR is an EPC manager object with a live
connection."
  (let ((proc (ignore-errors
                (key-echo-epc-connection-process (key-echo-epc-manager-connection mngr)))))
    (and (processp proc)
         ;; Same as `process-live-p' in Emacs >= 24:
         (memq (process-status proc) '(run open listen connect stop)))))

;; epcs
(defvar key-echo-epc-server-client-processes nil
  "[internal] A list of ([process object] . [`key-echo-epc-manager' instance]).
When the server process accepts the client connection, the
`key-echo-epc-manager' instance is created and stored in this variable
`key-echo-epc-server-client-processes'. This variable is used for the management
purpose.")

;; key-echo-epc-server
;;   name    : process name (string)   ex: "EPC Server 1"
;;   process : server process object
;;   port    : port number
;;   connect-function : initialize function for `key-echo-epc-manager' instances
(cl-defstruct key-echo-epc-server name process port connect-function)

(defvar key-echo-epc-server-processes nil
  "[internal] A list of ([process object] . [`key-echo-epc-server' instance]).
This variable is used for the management purpose.")

(defun key-echo-epc-server-get-manager-by-process (proc)
  "[internal] Return the key-echo-epc-manager instance for the PROC."
  (cl-loop for (pp . mngr) in key-echo-epc-server-client-processes
           if (eql pp proc)
           do (cl-return mngr)
           finally return nil))

(defun key-echo-epc-server-accept (process)
  "[internal] Initialize the process and return key-echo-epc-manager object."
  (key-echo-epc-log "LSPBRIDGE-EPC-SERVER- >> Connection accept: %S" process)
  (let* ((connection-id (key-echo-epc-uid))
         (connection-name (format "key-echo-epc con %s" connection-id))
         (channel (list connection-name nil))
         (connection (make-key-echo-epc-connection
                      :name connection-name
                      :process process
                      :buffer (process-buffer process)
                      :channel channel)))
    (key-echo-epc-log "LSPBRIDGE-EPC-SERVER- >> Connection establish")
    (set-process-coding-system process 'binary 'binary)
    (set-process-filter process
                        (lambda (p m)
                          (key-echo-epc-process-filter connection p m)))
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel process
                          (lambda (p e)
                            (key-echo-epc-process-sentinel connection p e)))
    (make-key-echo-epc-manager :server-process process :port t
                                :connection connection)))

(defun key-echo-epc-server-sentinel (process message connect-function)
  "[internal] Process sentinel handler for the server process."
  (key-echo-epc-log "LSPBRIDGE-EPC-SERVER- SENTINEL: %S %S" process message)
  (let ((mngr (key-echo-epc-server-get-manager-by-process process)))
    (cond
     ;; new connection
     ((and (string-match "open" message) (null mngr))
      (condition-case err
          (let ((mngr (key-echo-epc-server-accept process)))
            (push (cons process mngr) key-echo-epc-server-client-processes)
            (key-echo-epc-init-epc-layer mngr)
            (when connect-function (funcall connect-function mngr))
            mngr)
        ('error
         (key-echo-epc-log "LSPBRIDGE-EPC-SERVER- Protocol error: %S" err)
         (key-echo-epc-log "LSPBRIDGE-EPC-SERVER- ABORT %S" process)
         (delete-process process))))
     ;; ignore
     ((null mngr) nil )
     ;; disconnect
     (t
      (let ((pair (assq process key-echo-epc-server-client-processes)) _d)
        (when pair
          (key-echo-epc-log "LSPBRIDGE-EPC-SERVER- DISCONNECT %S" process)
          (key-echo-epc-stop-epc (cdr pair))
          (setq key-echo-epc-server-client-processes
                (assq-delete-all process key-echo-epc-server-client-processes))
          ))
      nil))))

(defun key-echo-epc-server-start (connect-function &optional port)
  "Start TCP Server and return the main process object."
  (let*
      ((connect-function connect-function)
       (name (format "KEY-ECHO EPC Server %s" (key-echo-epc-uid)))
       (buf (key-echo-epc-make-procbuf (format " *%s*" name)))
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
           (key-echo-epc-server-sentinel process message connect-function)))))
    (push (cons main-process
                (make-key-echo-epc-server
                 :name name :process main-process
                 :port (process-contact main-process :service)
                 :connect-function connect-function))
          key-echo-epc-server-processes)
    main-process))

(provide 'key-echo-epc)
;;; key-echo-epc.el ends here
