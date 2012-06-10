;;; font-face-test.el --- run the test for font faces of a major mode

;; Copyright (c) 2012 Takeshi Arabiki (abicky)

;; Author: Takeshi Arabiki (abicky)

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; Code:

(require 'cl)

(cd (file-name-directory load-file-name))
(load-file "../jsx-mode.el")
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))

(defvar jsx-dir "jsx/font-face")

(defun get-test-suite ()
  (when (string-match "^/\\*\\(?:.\\|\n\\)*\\*/" (buffer-string))
    (let ((beg (match-beginning 0))
          (end (match-end 0))
          test-suite token)
      (while (search-forward-regexp
              "\\([^ \t\n:][^\n:]*\\)\\s-*:\\s-*\\([-a-z]+\\)" end t)
        (setq token (match-string 1))
        (put-text-property 0 (length token) 'face (intern (match-string 2)) token)
        (setq test-suite (append test-suite (list token))))
      (goto-char end)
      (end-of-line)
      test-suite)))

(defun next-token ()
  (let ((buf (current-buffer)))
    (goto-char (or (next-property-change (point) buf) (point-max)))
    (while (and
            (not (get-text-property (point) 'face buf))
            (not (eobp)))
      (goto-char (or (next-property-change (point) buf) (point-max))))
    (if (eobp)
        nil
      (replace-regexp-in-string
       "[ \t\r\n]+$" ""
       (buffer-substring (point) (next-property-change (point) buf))))))

(defun run-test ()
  (let* ((cnt 0)
        (failed-cnt 0)
        (msg "")
        (start-time (current-time-string))
        (files (directory-files jsx-dir nil "\\.jsx\\'"))
        (total-cnt (length files)))
    (setq font-lock-verbose nil)
    (dolist (file files)
      (setq cnt (1+ cnt))
      (with-current-buffer (find-file-noselect (concat jsx-dir "/" file))
        (font-lock-default-fontify-buffer)
        (goto-char (point-min))
        (let ((test-suite (get-test-suite))
              (errors '())
              expected)
          (while (setq token (next-token))
            (setq expected (or (pop test-suite) ""))
            (condition-case ex
                (assert (and (equal expected token) (equal-face expected token)))
              (error
               (setq errors (append errors (list (make-error-msg expected token)))))))
          (when (> (length errors) 0)
            (setq msg (format "%sF %s:\n%s\n\n" msg file (mapconcat 'identity errors "\n\n")))
            (setq failed-cnt (1+ failed-cnt))))))
    (message
     (concat
      "Passed: " (format "%d\n" (- cnt failed-cnt))
      "Failed: " (format "%d\n" failed-cnt)
      "Total: " (format "%d/%d\n" cnt total-cnt)
      "\n"
      "Started at:  " start-time "\n"
      "Finished at: " (current-time-string) "\n"
      "\n"
      msg))))

(defun equal-face (a b)
  (eq (get-text-property 0 'face a) (get-text-property 0 'face b)))

(defun make-error-msg (expected actual)
  (format
   "\texpected: %s (%s)\n\tactual: %s (%s)"
   (substring-no-properties expected)
   (get-text-property 0 'face expected)
   (substring-no-properties actual)
   (get-text-property 0 'face actual)))

(run-test)
