;;; save-local-vars.el --- save buffer-local variables in visited file

;; Copyright (C) 2008-2012 Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20080830
;; Version: 0.1.1
;; Homepage: https://github.com/tarsius/save-local-vars
;; Keywords: convenience 

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Save the buffer-local binding of buffer-local variables
;; of the current buffer locally in the visited file.

;; I haven't use this package in a long time and just started fixing
;; it.  It is usable but there are a few things missing, which I
;; intend to fix later -- Jonas Bernoulli 20120801

;; TODO
;; * special handling of major modes
;; * special handling of minor modes
;; * special handling of eval forms
;; * dectect and handle other variable/value related edge-cases
;; * dectect and handle parsing edge-cases
;; * align comment-end if not an empty string
;; * use built-in code where possible
;; * mention `add-file-local-variable-prop-list'
;; * possibly rename to be more like ^^^
;; * don't just ignore property line
;; * make inserting  optional
;; * improve commentary
;; * I could go on

;;; Code:

(defvar save-local-variable-double-comment-start "[;]"
  "Regexp controlling whether `comment-start' should be inserted twice.
If the local value of `comment-start' matches this regular expression
`save-local-variable' inserts it twice at the beginning of each line
when the variable section is first created.")

(defun save-local-variable--variables-data ()
  "Return data identifying the file variable section, or nil if none."
  ;; Similar to `allout-file-vars-section-data' defined in `allout.el'.
  ;; Also see `hack-local-variables' defined in `files.el'.
  (let ((case-fold-search t))
    (save-excursion
      (goto-char (point-max))
      (search-backward "\n\^L" (max (- (point-max) 3000) (point-min)) 'move)
      (when (re-search-forward
             (format "^\\(%s+\\)[ \t]*Local Variables:\\(.*\\)$"
                     (regexp-quote comment-start))
             nil t)
        (let* ((prefix (match-string 1))
               (suffix (match-string 2))
               (begpos (line-end-position))
               (endpos
                (when (re-search-forward (format "^%s[ \t]*End:[ \t]*%s$"
                                                 (regexp-quote prefix)
                                                 (regexp-quote suffix))
                                         nil t)
                  (line-beginning-position))))
          (if endpos
              (list begpos endpos prefix suffix)
            (error "Local variables list is not properly terminated")))))))

(defun save-local-variable--print (variable)
  (insert comment-start
          (if (string-match " $" comment-start) "" " ")
          (symbol-name variable) ": "
          (prin1-to-string (symbol-value variable))
          comment-end "\n"))

(defun save-local-variable (&optional variable)
  "Save the current buffer-local value of VARIABLE in visited file."
  (interactive
   (let* ((v (variable-at-point))
          (enable-recursive-minibuffers t)
          (val (completing-read (if (and (symbolp v)
                                         (local-variable-p v))
                                    (format
                                     "Save variable (default %s): " v)
                                  "Save variable: ")
                                obarray
                                '(lambda (vv)
                                   (and (boundp vv)
                                        (local-variable-p vv)))
                                t nil nil
                                (if (and (symbolp v)
                                         (local-variable-p v))
                                    (symbol-name v)))))
     (list (if (equal val "") v
             (intern val)))))
  (unless (buffer-file-name)
    (error "Buffer isn't visiting a file: %s" (buffer-name)))
  (let ((standard-output (current-buffer))
        (modifiedp (buffer-modified-p))
        (data (save-local-variable--variables-data)))
    (save-excursion
      (if data
          (let ((begpos (nth 0 data))
                (endpos (nth 1 data))
                (comment-start (nth 2 data))
                (comment-end   (nth 3 data))
                varpos)
            (goto-char begpos)
            (if (re-search-forward
                 (format "^%s *%s:"
                         (regexp-quote comment-start)
                         (regexp-quote (symbol-name variable)))
                 endpos t)
                (progn (forward-line 0) (kill-line))
              (goto-char endpos))
            (save-local-variable--print variable))
        (if (re-search-forward (format "^%s+ .+ ends here"
                                       (regexp-quote comment-start))
                               nil t)
            (forward-line 0)
          (goto-char (buffer-end 1))
          (insert "\n\^L"))
        (unless (looking-back "^(provide '.+)\n")
          (insert "\n"))
        (let ((comment-start
               (concat comment-start
                       (and (string-match
                             save-local-variable-double-comment-start
                             comment-start)
                            comment-start)
                       (and (not (string-match " $" comment-start)) " "))))
          (insert comment-start "Local Variables:" comment-end "\n")
          (save-local-variable--print variable)
          (insert comment-start "End:" comment-end "\n"))))
    (when (or (not modifiedp)
              (y-or-n-p (format "Save %s? " (buffer-file-name))))
      (save-buffer))))

(provide 'save-local-vars)
;;; save-local-vars.el ends here
