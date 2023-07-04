;;; wraplish.el --- LSP bridge  -*- lexical-binding: t -*-

;; Filename: wraplish.el
;; Description: Add space between Chinese and English
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2023, Andy Stewart, all rights reserved.
;; Created: 2023-05-02 14:10:12
;; Version: 0.5
;; Last-Updated: 2023-05-02 15:23:53 +0800
;;           By: Andy Stewart
;; URL: https://github.com/manateelazycat/wraplish
;; Keywords:
;; Compatibility: emacs-version >= 28
;; Package-Requires: ((emacs "28")
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
;; Wraplish
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
;;      M-x customize-group RET wraplish RET
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
(require 'wraplish-epc)

(defgroup wraplish nil
  "Wraplish group."
  :group 'applications)

(defcustom wraplish-add-space-after-chinese-punctuation nil
  "Whether to add spaces after Chinese commas, default is nil"
  :type 'boolean
  :group 'wraplish)

(defcustom wraplish-add-space-before-markdown-link t
  "Whether to add spaces before markdown-link, default is non-nil"
  :type 'boolean
  :group 'wraplish)

(defvar wraplish-server nil
  "The Wraplish Server.")

(defvar wraplish-python-file (expand-file-name "wraplish.py" (if load-file-name
                                                                 (file-name-directory load-file-name)
                                                               default-directory)))

(defvar wraplish-server-port nil)

(defun wraplish--start-epc-server ()
  "Function to start the EPC server."
  (unless (process-live-p wraplish-server)
    (setq wraplish-server
          (wraplish-epc-server-start
           (lambda (mngr)
             (let ((mngr mngr))
               (wraplish-epc-define-method mngr 'eval-in-emacs 'wraplish--eval-in-emacs-func)
               (wraplish-epc-define-method mngr 'get-emacs-var 'wraplish--get-emacs-var-func)
               (wraplish-epc-define-method mngr 'get-emacs-vars 'wraplish--get-emacs-vars-func)
               (wraplish-epc-define-method mngr 'get-user-emacs-directory 'wraplish--user-emacs-directory)
               (wraplish-epc-define-method mngr 'get-buffer-content 'wraplish--get-buffer-content-func)
               ))))
    (if wraplish-server
        (setq wraplish-server-port (process-contact wraplish-server :service))
      (error "[Wraplish] wraplish-server failed to start")))
  wraplish-server)

(defun wraplish--eval-in-emacs-func (sexp-string)
  (eval (read sexp-string))
  ;; Return nil to avoid epc error `Got too many arguments in the reply'.
  nil)

(defun wraplish--get-emacs-var-func (var-name)
  (let* ((var-symbol (intern var-name))
         (var-value (symbol-value var-symbol))
         ;; We need convert result of booleanp to string.
         ;; Otherwise, python-epc will convert all `nil' to [] at Python side.
         (var-is-bool (prin1-to-string (booleanp var-value))))
    (list var-value var-is-bool)))

(defun wraplish--get-emacs-vars-func (&rest vars)
  (mapcar #'wraplish--get-emacs-var-func vars))

(defvar wraplish-epc-process nil)

(defvar wraplish-internal-process nil)
(defvar wraplish-internal-process-prog nil)
(defvar wraplish-internal-process-args nil)

(defcustom wraplish-name "*wraplish*"
  "Name of Wraplish buffer."
  :type 'string)

(defcustom wraplish-python-command (if (memq system-type '(cygwin windows-nt ms-dos)) "python.exe" "python3")
  "The Python interpreter used to run wraplish.py."
  :type 'string)

(defcustom wraplish-enable-debug nil
  "If you got segfault error, please turn this option.
Then Wraplish will start by gdb, please send new issue with `*wraplish*' buffer content when next crash."
  :type 'boolean)

(defcustom wraplish-enable-log nil
  "Enable this option to print log message in `*wraplish*' buffer, default only print message header."
  :type 'boolean)

(defcustom wraplish-enable-profile nil
  "Enable this option to output performance data to ~/wraplish.prof."
  :type 'boolean)

(defun wraplish--user-emacs-directory ()
  "Get lang server with project path, file path or file extension."
  (expand-file-name user-emacs-directory))

(defun wraplish-call-async (method &rest args)
  "Call Python EPC function METHOD and ARGS asynchronously."
  (if (wraplish-epc-live-p wraplish-epc-process)
      (wraplish-deferred-chain
        (wraplish-epc-call-deferred wraplish-epc-process (read method) args))
    (setq wraplish-first-call-method method)
    (setq wraplish-first-call-args args)
    (wraplish-start-process)))

(defvar wraplish-is-starting nil)
(defvar wraplish-first-call-method nil)
(defvar wraplish-first-call-args nil)

(defun wraplish-restart-process ()
  "Stop and restart Wraplish process."
  (interactive)
  (setq wraplish-is-starting nil)

  (wraplish-kill-process)
  (wraplish-start-process)
  (message "[Wraplish] Process restarted."))

(defun wraplish-start-process ()
  "Start Wraplish process if it isn't started."
  (setq wraplish-is-starting t)
  (unless (wraplish-epc-live-p wraplish-epc-process)
    ;; start epc server and set `wraplish-server-port'
    (wraplish--start-epc-server)
    (let* ((wraplish-args (append
                           (list wraplish-python-file)
                           (list (number-to-string wraplish-server-port))
                           (when wraplish-enable-profile
                             (list "profile"))
                           )))

      ;; Set process arguments.
      (if wraplish-enable-debug
          (progn
            (setq wraplish-internal-process-prog "gdb")
            (setq wraplish-internal-process-args (append (list "-batch" "-ex" "run" "-ex" "bt" "--args" wraplish-python-command) wraplish-args)))
        (setq wraplish-internal-process-prog wraplish-python-command)
        (setq wraplish-internal-process-args wraplish-args))

      ;; Start python process.
      (let ((process-connection-type t))
        (setq wraplish-internal-process
              (apply 'start-process
                     wraplish-name wraplish-name
                     wraplish-internal-process-prog wraplish-internal-process-args)))
      (set-process-query-on-exit-flag wraplish-internal-process nil))))

(defvar wraplish-stop-process-hook nil)

(defun wraplish-kill-process ()
  "Stop Wraplish process and kill all Wraplish buffers."
  (interactive)

  ;; Run stop process hooks.
  (run-hooks 'wraplish-stop-process-hook)

  ;; Kill process after kill buffer, make application can save session data.
  (wraplish--kill-python-process))

(add-hook 'kill-emacs-hook #'wraplish-kill-process)

(defun wraplish--kill-python-process ()
  "Kill Wraplish background python process."
  (when (wraplish-epc-live-p wraplish-epc-process)
    ;; Cleanup before exit Wraplish server process.
    (wraplish-call-async "cleanup")
    ;; Delete Wraplish server process.
    (wraplish-epc-stop-epc wraplish-epc-process)
    ;; Kill *wraplish* buffer.
    (when (get-buffer wraplish-name)
      (kill-buffer wraplish-name))
    (setq wraplish-epc-process nil)
    (message "[Wraplish] Process terminated.")))

(defun wraplish--first-start (wraplish-epc-port)
  "Call `wraplish--open-internal' upon receiving `start_finish' signal from server."
  ;; Make EPC process.
  (setq wraplish-epc-process (make-wraplish-epc-manager
                              :server-process wraplish-internal-process
                              :commands (cons wraplish-internal-process-prog wraplish-internal-process-args)
                              :title (mapconcat 'identity (cons wraplish-internal-process-prog wraplish-internal-process-args) " ")
                              :port wraplish-epc-port
                              :connection (wraplish-epc-connect "127.0.0.1" wraplish-epc-port)
                              ))
  (wraplish-epc-init-epc-layer wraplish-epc-process)
  (setq wraplish-is-starting nil)

  (when (and wraplish-first-call-method
             wraplish-first-call-args)
    (wraplish-deferred-chain
      (wraplish-epc-call-deferred wraplish-epc-process
                                  (read wraplish-first-call-method)
                                  wraplish-first-call-args)
      (setq wraplish-first-call-method nil)
      (setq wraplish-first-call-args nil)
      )))

;;;###autoload
(define-minor-mode wraplish-mode
  "Wraplish mode."
  :init-value nil
  (if wraplish-mode
      (wraplish--enable)
    (wraplish--disable)))

(defun wraplish--enable ()
  "Enable Wraplish mode."
  (setq-local wraplish-revert-buffer-flag nil)
  (setq-local wraplish-insert-space-flag nil)

  (dolist (hook wraplish--internal-hooks)
    (apply #'add-hook hook))

  ;; Flag `wraplish-is-starting' make sure only call `wraplish-start-process' once.
  (unless wraplish-is-starting
    (wraplish-start-process)))

(defun wraplish--disable ()
  "Disable Wraplish mode."
  ;; Remove hooks.
  (dolist (hook wraplish--internal-hooks)
    (remove-hook (nth 0 hook) (nth 1 hook) (nth 3 hook))))

(defconst wraplish--internal-hooks
  '((before-change-functions wraplish-monitor-before-change nil t)
    (after-change-functions wraplish-monitor-after-change nil t)
    (kill-buffer-hook wraplish-close-buffer nil t)
    (before-revert-hook wraplish-close-buffer nil t)))

(defvar-local wraplish--before-change-begin-pos nil)
(defvar-local wraplish--before-change-end-pos nil)

(defvar-local wraplish--sync-flag nil)

(defun wraplish--point-position (pos)
  "Get position of POS."
  (save-excursion
    (goto-char pos)
    (wraplish--position)))

(defun wraplish--calculate-column ()
  "Calculate character offset of cursor in current line."
  (/ (- (length (encode-coding-region (line-beginning-position)
                                      (min (point) (point-max)) 'utf-16 t))
        2)
     2))

(defun wraplish--position ()
  "Get position of cursor."
  ;; we should use ABSOLUTE line number to be compatible with narrowed buffer
  (list :line (1- (line-number-at-pos nil t))
        :character (wraplish--calculate-column)))

(defun wraplish-monitor-before-change (begin end)
  (setq-local wraplish--before-change-begin-pos (wraplish--point-position begin))
  (setq-local wraplish--before-change-end-pos (wraplish--point-position end)))

(defun wraplish-monitor-after-change (begin end length)
  (when (and (not wraplish-revert-buffer-flag)
             (not wraplish-insert-space-flag))
    (let ((this-command-string (format "%s" this-command)))
      (if (or
           ;; Don't send `change_file' request if current command match blacklist.
           (member this-command-string '("query-replace" "undo" "undo-redo" "undo-tree-undo" "undo-tree-redo"))
           ;; Don't send `change_file' request if current command is vundo command.
           (string-prefix-p "vundo" this-command-string)
           ;; Don't send `change_file' request if current command is delete command.
           (> length 0))
          ;; Set `wraplish--sync-flag' to non-nil to sync file in next loop.
          (setq-local wraplish--sync-flag t)

        (wraplish-call-async "change_buffer"
                             (buffer-name)
                             wraplish--before-change-begin-pos
                             wraplish--before-change-end-pos
                             (buffer-substring-no-properties begin end)
                             (buffer-chars-modified-tick)
                             wraplish--sync-flag
                             )

        (setq-local wraplish--sync-flag nil)))))

(defun wraplish-close-buffer ()
  (wraplish-call-async "close_buffer" (buffer-name)))

(defun wraplish--get-buffer-content-func (buffer-name)
  "Get buffer content for lsp. BUFFER-NAME is name eval from (buffer-name)."
  (when-let* ((buf (get-buffer buffer-name)))
    (with-current-buffer buf
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun wraplish-insert-spaces (buffer-name space-positions)
  (setq-local wraplish-insert-space-flag t)

  (save-excursion
    (with-current-buffer buffer-name
      (dolist (position (reverse space-positions))
        (goto-char (1+ position))
        (unless (equal (char-after) ?\ )
          (insert " ")))))

  (wraplish-call-async "sync_buffer" buffer-name)

  (setq-local wraplish-insert-space-flag nil))

;; We use `wraplish-revert-buffer-flag' var avoid wraplish send change_file request while execute `revert-buffer' command.
(defun wraplish--revert-buffer-advisor (orig-fun &optional arg &rest args)
  (setq-local wraplish-revert-buffer-flag t)
  (apply orig-fun arg args)
  (setq-local wraplish-revert-buffer-flag nil))
(advice-add #'revert-buffer :around #'wraplish--revert-buffer-advisor)

(provide 'wraplish)

;;; wraplish.el ends here
