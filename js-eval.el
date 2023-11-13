;;; js-eval.el --- eval javascript directly from buffer -*- lexical-binding: t; -*-
;;;
;;; Copyright (C) 2023 Bruno Dias.
;;;
;;; Author: Bruno Dias <dias.h.bruno@gmail.com>
;;; Version: 1.0
;;; Package-Requires: ((s "1.13.1"))
;;; Keywords: multimedia, hypermedia
;;; URL: https://github.com/diasbruno/js-eval.el
;;;
;;; Commentary:
;;;
;;; Code:

(require 's)

(defvar js-eval--process-output-buffer nil)
(defvar js-eval--repl-process nil)
(defvar js-eval--reading-process-buffer "")
(defvar js-eval--process-name "js-eval")
(defvar js-eval--repl-buffer-name "*js-eval output*")
(defvar js-eval--nodejs-program "node")

(defun js-eval--start-repl ()
  "Start a new repl process."
  (with-environment-variables (("NODE_DISABLE_COLORS" "1"))
   (start-process js-eval--process-name
                  js-eval--process-output-buffer
                  js-eval--nodejs-program)))

(defun js-eval ()
  "Initialize a new repl."
  (interactive)
  (when (null js-eval--repl-process)
    (setf js-eval--process-output-buffer
          (generate-new-buffer js-eval--repl-buffer-name)
          js-eval--repl-process
          (js-eval--start-repl))
    (set-process-filter js-eval--repl-process 'js-eval--pipe-output)))

(defun js-eval-quit ()
  "Terminate the repl process."
  (interactive)
  (when js-eval--repl-process
    (kill-process js-eval--repl-process)
    (kill-buffer js-eval--process-output-buffer)
    (setf js-eval--process-output-buffer nil
          js-eval--repl-process nil)))

(defun js-eval-eval-region (st en)
  "Eval region from start (ST) till (EN)."
  (interactive "r")
  (let ((str (buffer-substring st en)))
    (kill-region st en)
    (insert str)
    (process-send-string js-eval--repl-process (concat str "\n"))))

(defun js-eval-eval-expression ()
  "Eval expression."
  (interactive)
  (let ((a (point)))
    (save-excursion
      (backward-paragraph)
      (process-send-string
       js-eval--repl-process
       (concat (buffer-substring a (point)) "\n")))))

(defun js-eval--accumulate-in-buffer (out)
  "Accumulate out (OUT) on process buffer."
  (setf js-eval--reading-process-buffer
        (concat js-eval--reading-process-buffer out)))

(defun js-eval--clean-process-buffer ()
  "Clean process buffer."
  (setf js-eval--reading-process-buffer ""))

(defun js-eval--write-on-process-buffer (buf)
  "Write on process buffer (BUF)."
  (with-current-buffer js-eval--process-output-buffer
    (goto-char (point-max))
    (insert buf)
    (goto-char (point-max))))

(defun js-eval--process-user-message ()
  "The message we sent to execute."
  (js-eval--write-on-process-buffer
   js-eval--reading-process-buffer))

(defun js-eval--process-repl-response ()
  "The message received from the ."
  (let ((msg (car (s-split "\n>" js-eval--reading-process-buffer))))
    (js-eval--write-on-process-buffer js-eval--reading-process-buffer)
    (with-current-buffer (current-buffer)
      (insert "\n")
      (insert (concat "// " (replace-regexp-in-string
                             "\n" "\n// "
                             (replace-regexp-in-string "" "" msg))))
      (insert "\n"))))

(defun js-eval--pipe-output (proc out)
  "Read (OUT) from process (PROC) the messages from the repl process."
  (let ((end-message (s-contains? "" out)))
    (js-eval--accumulate-in-buffer out)
    (when end-message
      (if (s-contains? "" out)
          (js-eval--process-user-message)
        (js-eval--process-repl-response))
      (js-eval--clean-process-buffer))))

(provide 'js-eval)
;;; js-eval.el ends here
