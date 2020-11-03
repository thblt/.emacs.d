(defun bench-buffer (&optional buffer)
  (interactive)
  (setq buffer (or buffer (current-buffer)))
  (with-current-buffer buffer
    (goto-char (point-min))
    (remove-overlays (point-min) (point-max))
    (while (not (eobp))
      (forward-sexp)
      (let ((time (current-time))
	          (overlay (make-overlay (point) (point))))
	      (eval-last-sexp nil)
	      (overlay-put overlay 'after-string (format " => Spent %4fs" (float-time (time-since time))))))))

(define-key global-map (kbd "<f5>") 'bench-buffer)
