;;; key-echo.el --- LSP bridge  -*- lexical-binding: t -*-

;; Filename: key-echo.el
;; Description: LSP bridge
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-06-15 14:10:12
;; Version: 0.5
;; Last-Updated: 2022-10-10 15:23:53 +0800
;;           By: Gong Qijian
;; URL: https://github.com/manateelazycat/key-echo
;; Keywords:
;; Compatibility: emacs-version >= 28
;; Package-Requires: ((emacs "28") (posframe "1.1.7") (markdown-mode "2.6"))
;;
;; Features that might be required by this library:
;;
;; Please check README
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Key-Echo
;;

;;; Installation:
;;
;; Please check README
;;

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET key-echo RET
;;

;;; Change log:
;;
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'key-echo-epc)

(defgroup key-echo nil
  "Key-Echo group."
  :group 'applications)

(defcustom key-echo-keyboard-quit-key "Key.alt_r"
  "`keyboard-quit' is binding to Ctrl + g in Emacs.

We can't call function `keyboard-quit' to implement same effect like Ctrl + g.

So we use right Alt to send key `C-g' to Emacs.")

(defvar key-echo-server nil
  "The Key-Echo Server.")

(defvar key-echo-python-file (expand-file-name "key_echo.py" (if load-file-name
                                                                 (file-name-directory load-file-name)
                                                               default-directory)))

(defvar key-echo-server-port nil)

(defcustom key-echo-single-key-trigger-func nil
  "Method to call when single key trigger.")

(defun key-echo--start-epc-server ()
  "Function to start the EPC server."
  (unless (process-live-p key-echo-server)
    (setq key-echo-server
          (key-echo-epc-server-start
           (lambda (mngr)
             (let ((mngr mngr))
               (key-echo-epc-define-method mngr 'eval-in-emacs 'key-echo--eval-in-emacs-func)
               (key-echo-epc-define-method mngr 'get-emacs-var 'key-echo--get-emacs-var-func)
               (key-echo-epc-define-method mngr 'get-emacs-vars 'key-echo--get-emacs-vars-func)
               (key-echo-epc-define-method mngr 'get-user-emacs-directory 'key-echo--user-emacs-directory)
               (key-echo-epc-define-method mngr 'get-emacs-id 'key-echo--get-emacs-id)
	       (key-echo-epc-define-method mngr 'get-emacs-pid 'emacs-pid)
	       (key-echo-epc-define-method mngr 'emacs-running-in-wayland-native 'key-echo-emacs-running-in-wayland-native)
               ))))
    (if key-echo-server
        (setq key-echo-server-port (process-contact key-echo-server :service))
      (error "[Key-Echo] key-echo-server failed to start")))
  key-echo-server)

(defun key-echo--eval-in-emacs-func (sexp-string)
  (eval (read sexp-string))
  ;; Return nil to avoid epc error `Got too many arguments in the reply'.
  nil)

(defun key-echo--get-emacs-var-func (var-name)
  (let* ((var-symbol (intern var-name))
         (var-value (symbol-value var-symbol))
         ;; We need convert result of booleanp to string.
         ;; Otherwise, python-epc will convert all `nil' to [] at Python side.
         (var-is-bool (prin1-to-string (booleanp var-value))))
    (list var-value var-is-bool)))

(defun key-echo--get-emacs-vars-func (&rest vars)
  (mapcar #'key-echo--get-emacs-var-func vars))

(defvar key-echo-epc-process nil)

(defvar key-echo-internal-process nil)
(defvar key-echo-internal-process-prog nil)
(defvar key-echo-internal-process-args nil)

(defcustom key-echo-name "*key-echo*"
  "Name of Key-Echo buffer."
  :type 'string)

(defcustom key-echo-python-command (if (memq system-type '(cygwin windows-nt ms-dos)) "python.exe" "python3")
  "The Python interpreter used to run key_echo.py."
  :type 'string)

(defcustom key-echo-enable-debug nil
  "If you got segfault error, please turn this option.
Then Key-Echo will start by gdb, please send new issue with `*key-echo*' buffer content when next crash."
  :type 'boolean)

(defcustom key-echo-enable-log nil
  "Enable this option to print log message in `*key-echo*' buffer, default only print message header."
  :type 'boolean)

(defcustom key-echo-enable-profile nil
  "Enable this option to output performance data to ~/key-echo.prof."
  :type 'boolean)

(defun key-echo--user-emacs-directory ()
  "Get lang server with project path, file path or file extension."
  (expand-file-name user-emacs-directory))

(defun key-echo-call-async (method &rest args)
  "Call Python EPC function METHOD and ARGS asynchronously."
  (if (key-echo-epc-live-p key-echo-epc-process)
      (key-echo-deferred-chain
        (key-echo-epc-call-deferred key-echo-epc-process (read method) args))
    (setq key-echo-first-call-method method)
    (setq key-echo-first-call-args args)
    (key-echo-start-process)))

(defvar key-echo-is-starting nil)
(defvar key-echo-first-call-method nil)
(defvar key-echo-first-call-args nil)

(defun key-echo-restart-process ()
  "Stop and restart Key-Echo process."
  (interactive)
  (setq key-echo-is-starting nil)

  (key-echo-kill-process)
  (key-echo-start-process)
  (message "[Key-Echo] Process restarted."))

(defun key-echo-start-process ()
  "Start Key-Echo process if it isn't started."
  (setq key-echo-is-starting t)
  (if (key-echo-epc-live-p key-echo-epc-process)
      (remove-hook 'post-command-hook #'key-echo-start-process)
    ;; start epc server and set `key-echo-server-port'
    (key-echo--start-epc-server)
    (let* ((key-echo-args (append
                           (list key-echo-python-file)
                           (list (number-to-string key-echo-server-port))
                           (when key-echo-enable-profile
                             (list "profile"))
                           )))

      ;; Set process arguments.
      (if key-echo-enable-debug
          (progn
            (setq key-echo-internal-process-prog "gdb")
            (setq key-echo-internal-process-args (append (list "-batch" "-ex" "run" "-ex" "bt" "--args" key-echo-python-command) key-echo-args)))
        (setq key-echo-internal-process-prog key-echo-python-command)
        (setq key-echo-internal-process-args key-echo-args))

      ;; Start python process.
      (let ((process-connection-type t))
        (setq key-echo-internal-process
              (apply 'start-process
                     key-echo-name key-echo-name
                     key-echo-internal-process-prog key-echo-internal-process-args)))
      (set-process-query-on-exit-flag key-echo-internal-process nil))))

(defvar key-echo-stop-process-hook nil)

(defun key-echo-kill-process ()
  "Stop Key-Echo process and kill all Key-Echo buffers."
  (interactive)

  ;; Run stop process hooks.
  (run-hooks 'key-echo-stop-process-hook)

  ;; Kill process after kill buffer, make application can save session data.
  (key-echo--kill-python-process))

(add-hook 'kill-emacs-hook #'key-echo-kill-process)

(defun key-echo--kill-python-process ()
  "Kill Key-Echo background python process."
  (when (key-echo-epc-live-p key-echo-epc-process)
    ;; Cleanup before exit Key-Echo server process.
    (key-echo-call-async "cleanup")
    ;; Delete Key-Echo server process.
    (key-echo-epc-stop-epc key-echo-epc-process)
    ;; Kill *key-echo* buffer.
    (when (get-buffer key-echo-name)
      (kill-buffer key-echo-name))
    (setq key-echo-epc-process nil)
    (message "[Key-Echo] Process terminated.")))

(defun key-echo--first-start (key-echo-epc-port)
  "Call `key-echo--open-internal' upon receiving `start_finish' signal from server."
  ;; Make EPC process.
  (setq key-echo-epc-process (make-key-echo-epc-manager
                              :server-process key-echo-internal-process
                              :commands (cons key-echo-internal-process-prog key-echo-internal-process-args)
                              :title (mapconcat 'identity (cons key-echo-internal-process-prog key-echo-internal-process-args) " ")
                              :port key-echo-epc-port
                              :connection (key-echo-epc-connect "127.0.0.1" key-echo-epc-port)
                              ))
  (key-echo-epc-init-epc-layer key-echo-epc-process)
  (setq key-echo-is-starting nil)

  (when (and key-echo-first-call-method
             key-echo-first-call-args)
    (key-echo-deferred-chain
      (key-echo-epc-call-deferred key-echo-epc-process
                                  (read key-echo-first-call-method)
                                  key-echo-first-call-args)
      (setq key-echo-first-call-method nil)
      (setq key-echo-first-call-args nil)
      )))

(defun key-echo--get-emacs-id ()
  (if (eq system-type 'darwin)
      (emacs-pid)
    (string-to-number (frame-parameter nil 'outer-window-id))))

(defun key-echo-single-key-trigger (key)
  (when key-echo-single-key-trigger-func
    (funcall key-echo-single-key-trigger-func key)))

(defun key-echo-emacs-running-in-wayland-native ()
  (eq window-system 'pgtk))

(defun key-echo-enable ()
  (add-hook 'post-command-hook #'key-echo-start-process))

(unless key-echo-is-starting
  (key-echo-start-process))

(provide 'key-echo)

;;; key-echo.el ends here
