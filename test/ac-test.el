(defun jsx-test-propertize (json)
  (let ((objs (json-read-from-string json))
        candidates)
    (while objs
      (let* ((obj (car objs))
             (word (assoc-default 'word obj))
             (docs (assoc-default 'docs obj))
             (symbol (assoc-default 'symbol obj))
             (candidate (propertize word 'docs docs 'symbol symbol)))
        (setq candidates (append candidates (list candidate)))
        (setq objs (cdr objs))))
    candidates))

(defun jsx-test-make-docs (json)
  (let ((candidates (jsx-test-propertize json)))
    (loop for candidate in candidates
          collect (jsx--get-document candidate))))

(defun ac-parse-next (regex)
  (search-forward-regexp regex (point-max) t)
  (let ((funcname (buffer-substring (point) (point-at-eol))))
    (search-forward-regexp "[[{]" (point-max) t)
    (backward-char)
    (funcall (intern funcname)
             (buffer-substring (point) (progn (forward-list) (point))))))

(defun ac-get-test-suite ()
  (let (expecteds actuals funcname)
    (while (not (eobp))
      (push-end (ac-parse-next "^input:\\s-*") actuals)
      (push-end (ac-parse-next "^expected:\\s-*") expecteds)
      (if (not (search-forward-regexp "\\s-+" (point-max) t))
          (goto-char (point-max))))
    (cons expecteds actuals)))

(defun compare-texts (expected actual)
  (setq expected (sort expected 'string<))
  (setq actual (sort actual 'string<))
  (not
   (loop for i from 0 to (1- (length expected))
         for exp = (nth i expected)
         for act = (nth i actual)
         ;; `equal-including-properties' uses `eq' to compare strings
         thereis (not (compare-text exp act)))))

(defun compare-text (a b)
  (and
   (equal a b)
   (equal
    (get-text-property 0 'symbol a)
    (get-text-property 0 'symbol b))
   (equal
    (get-text-property 0 'docs a)
    (get-text-property 0 'docs b))))

(defun make-error-msgs (expected actual)
  (setq expected (sort expected 'string<))
  (setq actual (sort actual 'string<))
  (loop with (eelems aelems)
        for i from 0 to (1- (length expected))
        for exp = (nth i expected)
        for act = (nth i actual)
        when (not (compare-text exp act))
        collect (prin1-to-string exp) into eelems
        and collect (prin1-to-string act) into aelems

        finally do
        (let ((format (mapconcat (lambda (str) "%s") eelems "\n\t\t")))
          (return (cons
                   (apply 'format (append (list format) eelems))
                   (apply 'format (append (list format) aelems)))))))

(setq json-array-type 'list)

(cd (file-name-directory load-file-name))
(load (expand-file-name "./lib/test-util.el") nil t t)
(load (expand-file-name "../src/jsx-mode.el") nil t t)

(if (run-test "ac" "txt"
              :get-test-suite-func 'ac-get-test-suite
              :make-msg-func 'make-error-msgs
              :compare-func 'compare-texts)
    (kill-emacs)
  (kill-emacs 1))
