;;; reason-mode.el --- A major emacs mode for editing Reason (based on rust-mode) -*-lexical-binding: t-*-
;; Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.

;;; Commentary:

;; Indentation functions for Reason.

;;; Code:

(defcustom reason-indent-offset 2
  "Indent Reason code by this number of spaces."
  :type 'integer
  :group 'reason-mode
  :safe #'integerp)

(defun reason-looking-back-str (str)
  "Like `looking-back' but for fixed strings rather than regexps (so that it's not so slow)"
  (let ((len (length str)))
    (and (> (point) len)
         (equal str (buffer-substring-no-properties (- (point) len) (point))))))

(defun reason-paren-level () (nth 0 (syntax-ppss)))
(defun reason-in-str-or-cmnt () (nth 8 (syntax-ppss)))
(defun reason-rewind-past-str-cmnt () (goto-char (nth 8 (syntax-ppss))))
(defun reason-rewind-irrelevant ()
  (interactive)
  (let ((starting (point)))
    (skip-chars-backward "[:space:]\n")
    (if (reason-looking-back-str "*/") (backward-char))
    (if (reason-in-str-or-cmnt)
        (reason-rewind-past-str-cmnt))
    (if (/= starting (point))
        (reason-rewind-irrelevant))))

(defun reason-align-to-expr-after-brace ()
  (save-excursion
    (forward-char)
    ;; We don't want to indent out to the open bracket if the
    ;; open bracket ends the line
    (when (not (looking-at "[[:blank:]]*\\(?://.*\\)?$"))
      (when (looking-at "[[:space:]]")
        (forward-word 1)
        (backward-word 1))
      (current-column))))


;;; Start of a reason binding
(defvar reason-binding
  (regexp-opt '("let" "type")))

(defun reason-beginning-of-defun (&optional arg)
  "Move backward to the beginning of the current defun.

With ARG, move backward multiple defuns.  Negative ARG means
move forward.

This is written mainly to be used as `beginning-of-defun-function'.
Don't move to the beginning of the line. `beginning-of-defun',
which calls this, does that afterwards."
  (interactive "p")
  (re-search-backward (concat "^\\(" reason-binding "\\)\\_>")
                      nil 'move (or arg 1)))

(defun reason-end-of-defun ()
  "Move forward to the next end of defun.

With argument, do it that many times.
Negative argument -N means move back to Nth preceding end of defun.

Assume that this is called after beginning-of-defun. So point is
at the beginning of the defun body.

This is written mainly to be used as `end-of-defun-function' for Reason."
  (interactive)
  ;; Find the opening brace
  (if (re-search-forward "[{]" nil t)
      (progn
        (goto-char (match-beginning 0))
        ;; Go to the closing brace
        (condition-case nil
            (forward-sexp)
          (scan-error
           ;; The parentheses are unbalanced; instead of being unable to fontify, just jump to the end of the buffer
           (goto-char (point-max)))))
    ;; There is no opening brace, so consider the whole buffer to be one "defun"
    (goto-char (point-max))))


(defun reason-rewind-to-beginning-of-current-level-expr ()
  (interactive)
  (let ((current-level (reason-paren-level)))
    (back-to-indentation)
    (when (looking-at "->")
      (reason-rewind-irrelevant)
      (back-to-indentation))
    (while (> (reason-paren-level) current-level)
      (backward-up-list)
      (back-to-indentation))
    ;; When we're in the where clause, skip over it.  First find out the start
    ;; of the function and its paren level.
    (let ((function-start nil) (function-level nil))
      (save-excursion
        (reason-beginning-of-defun)
        (back-to-indentation)
        ;; Avoid using multiple-value-bind
        (setq function-start (point)
              function-level (reason-paren-level)))
      ;; On a where clause
      (when (or (looking-at "\\bwhere\\b")
                ;; or in one of the following lines, e.g.
                ;; where A: Eq
                ;;       B: Hash <- on this line
                (and (save-excursion
                       (re-search-backward "\\bwhere\\b" function-start t))
                     (= current-level function-level)))
        (goto-char function-start)))))



(defun reason-mode-indent-line () ())

(provide 'reason-indent)

;;; reason-indent.el ends here
