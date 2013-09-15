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

;;; Commentary:

;; This file is currently for my private use.

;;; Code:

(defun font-face-test-get-test-suite ()
  (when (string-match "^/\\*\\(?:.\\|\n\\)*?\\*/" (buffer-string))
    (let ((beg (match-beginning 0))
          (end (match-end 0))
          expecteds actuals token)
      (while (search-forward-regexp
              "\\([^ \t\n:][^\n:]*\\)\\s-*:\\s-*\\([-a-z]+\\)" end t)
        (setq token (match-string 1))
        (put-text-property 0 (length token) 'face (intern (match-string 2)) token)
        (push-end token expecteds))
      (goto-char end)
      (while (setq token (next-token))
        (push-end token actuals))
      (cons expecteds actuals))))

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
       (buffer-substring (point) (or (next-property-change (point) buf) (point-max)))))))

(defun equal-face (a b)
  (eq (get-text-property 0 'face a) (get-text-property 0 'face b)))

(defun compare-token (expected actual)
  (and (equal expected actual) (equal-face expected actual)))

(defun font-face-test-pre-func ()
  (custom-set-variables '(font-lock-verbose nil))
  (font-lock-default-fontify-buffer)
  (goto-char (point-min)))

(defun make-error-msgs (expected actual)
  (cons
   (format "%s (%s)"
           (and expected (substring-no-properties expected))
           (and expected (get-text-property 0 'face expected)))
   (format "%s (%s)"
           (and actual (substring-no-properties actual))
           (and actual (get-text-property 0 'face actual)))))


(setq vc-handled-backends nil)
(cd (file-name-directory load-file-name))
(load (expand-file-name "./lib/test-util.el") nil t t)
(load (expand-file-name "../src/jsx-mode.el") nil t t)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))

(if (run-test "jsx/font-face" "jsx"
              :pre-func 'font-face-test-pre-func
              :get-test-suite-func 'font-face-test-get-test-suite
              :make-msg-func 'make-error-msgs
              :compare-func 'compare-token)
    (kill-emacs)
  (kill-emacs 1))
