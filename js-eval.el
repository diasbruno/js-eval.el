(defvar *no* nil)
(defvar *np* nil)
(defvar *r* nil)
(defvar p 0)
(defvar b "")

(defun jsrepl ()
  (interactive)
  (when (null *np*)
    (setf *no* (generate-new-buffer "*js-repl output*"))
    (setf *np* (start-process "jsrepl"
                              *no*
                              "node"))
    (set-process-filter *np* 'jsrepl-pipe-output)))

(defun jsrepl-quit ()
  (interactive)
  (when (not (null *np*))
    (setf *r* nil)
    (kill-process *np*)
    (kill-buffer *no*)
    (setf *no* nil)
    (setf *np* nil)))

(defun jsrepl-eval-region (st en)
  (interactive "r")
  (let ((str (buffer-substring st en)))
    (kill-region st en)
    (insert str)
    (process-send-string *np* (concat str "\n"))))

(defun jsrepl-eval-expression ()
  (interactive)
  (let ((a (point))
        (b nil))
    (save-excursion
      (backward-paragraph)
      (process-send-string *np* (concat (buffer-substring a (point))
                                        "\n")))))

(defun jsrepl-process-node-output (out)
  (let ((maybe-user nil)
        (user-input nil)
        (node-output nil)
        (current nil))
    (mapcar (lambda (c)
              (if (= c 13)
                    (if maybe-user
                        (progn
                          (setf user-input (concat user-input current))
                          (setf current nil))
                      (progn

                        (setf maybe-user t)))
                  (if maybe-user
                      (progn
                        (setf node-output (concat node-output current))
                        (setf current nil)
                        (setq maybe-user nil))
                    (setq current (concatenate 'string current
                                                 (string c))))))
            out)
    (prog1 (cons user-input node-output)
      (message "user: %s\n node: %s\n" user-input node-output))))

(defun jsrepl-pipe-output (proc out)
  (let* ((a (replace-regexp-in-string "\[[0-9]+[A-Z]" "" out nil nil))
         (l (length a)))
    (when (and (> l 0)
               (> (length (replace-regexp-in-string "[
\n]" "" out t nil))
                  0))
      (progn
        (message "1 %s %d %s" out l a)
        (if (not *r*)
            (setf *r* t)
          (progn
            (setf b (concat b a))
            (message "2 %s" b)
            (when (string-equal "> " (substring b (- l 2) l))
              (progn
                (message "processing %s" (substring b 2))
               (let ((output (jsrepl-process-node-output (substring b 2)))
                     (cb (current-buffer)))
                 (progn
                   (when (cdr output)
                     (with-current-buffer cb
                       (insert "\n")
                       (insert (concat "// " (cdr output)))
                       (insert "\n\n")))
                   (when (car output)
                     (with-current-buffer *no*
                       (insert (car output))
                       (insert "\n")
                       (goto-char (point-max))))
                   (setf b "")))))))))))