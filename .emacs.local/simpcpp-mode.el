;;
;; Currently only a minor mode for C++, to handle indentations
;;
;; Makes use of the simpc indentation for C++
;; Highlighting is going to be as per the default major mode


;; ******************************************************
;;                   From simpc-mode.el

(defun simpc--previous-non-empty-line ()
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp))
                (string-empty-p
                 (string-trim-right
                  (thing-at-point 'line t))))
      (forward-line -1))
    (thing-at-point 'line t)))

(defun simpc--indentation-of-previous-non-empty-line ()
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp))
                (string-empty-p
                 (string-trim-right
                  (thing-at-point 'line t))))
      (forward-line -1))
    (current-indentation)))

(defun simpc--desired-indentation ()
  (let* ((cur-line (string-trim-right (thing-at-point 'line t)))
         (prev-line (string-trim-right (simpc--previous-non-empty-line)))
         (indent-len 4)
         (prev-indent (simpc--indentation-of-previous-non-empty-line)))
    (cond
     ((string-match-p "^\\s-*switch\\s-*(.+)" prev-line)
      prev-indent)
     ((and (string-suffix-p "{" prev-line)
           (string-prefix-p "}" (string-trim-left cur-line)))
      prev-indent)
     ((string-suffix-p "{" prev-line)
      (+ prev-indent indent-len))
     ((string-prefix-p "}" (string-trim-left cur-line))
      (max (- prev-indent indent-len) 0))
     ((string-suffix-p ":" prev-line)
      (if (string-suffix-p ":" cur-line)
          prev-indent
        (+ prev-indent indent-len)))
     ((string-suffix-p ":" cur-line)
      (max (- prev-indent indent-len) 0))
     (t prev-indent))))

;;; TODO: customizable indentation (amount of spaces, tabs, etc)
(defun simpc-indent-line ()
  (interactive)
  (when (not (bobp))
    (let* ((desired-indentation
            (simpc--desired-indentation))
           (n (max (- (current-column) (current-indentation)) 0)))
      (indent-line-to desired-indentation)
      (forward-char n))))

;;             END of simpc-mode.el code
;; ***************************************************

(define-minor-mode simpcpp-indent-mode
  "Minor mode to use `simpc-indent-line` for indentation."
  :lighter " Simple C++ Indent"
  (if simpcpp-indent-mode
      ;; When enabled
      (setq-local indent-line-function #'simpc-indent-line)
    ;; When disabled, restore default from c++-mode
    (kill-local-variable 'indent-line-function)))

(provide 'simpcpp-mode)
