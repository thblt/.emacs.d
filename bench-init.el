(defun cmpcar (a b)
  "Return non-nil is the car of A is smaller than the car or B."
  (> (car a) (car b)))

(defmacro eval-when-compile (&rest _))

(defun bench-buffer (&optional buffer)
  (interactive)
  (setq buffer (or buffer (current-buffer)))
  (let ((benchmark))
    (with-current-buffer buffer
      (goto-char (point-min))
      (remove-overlays (point-min) (point-max))
      (while (not (eobp))
        (forward-sexp)
        (let ((time (current-time)))
		      (eval-last-sexp nil)
          (push (cons (float-time (time-since time)) (point)) benchmark)
          )))
    ;; Print results
    (with-current-buffer (get-buffer-create "*Bench*")
      (org-mode)
      (insert "| Time | Point |\n")
      (insert "|------|-------|\n")
      (mapc (lambda (x)
              (insert (format"| %4fs | %i |\n" (car x) (cdr x))))
            (sort benchmark 'cmpcar)
            )
      (org-table-align))))

(define-key global-map (kbd "<f5>") 'bench-buffer)
