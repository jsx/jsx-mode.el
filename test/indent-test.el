;;; indent-test.el --- run the test for indentations of a major mode

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

(defun indent-test-get-test-suite ()
    (let (expecteds actuals)
      (while (not (eobp))
        (beginning-of-line)
        (when (search-forward-regexp "[0-9]+$" (point-at-eol) t)
          (push-end
           (list (string-to-number (match-string 0))
                 (buffer-substring (point-at-bol) (point-at-eol))
                 (line-number-at-pos))
           expecteds)
          (push-end (funcall calculate-indent-func) actuals))
        (forward-line))
      (cons expecteds actuals)))

(defun indent-test-pre-func ()
  (custom-set-variables '(font-lock-verbose nil))
  (goto-char (point-min)))

(defun compare-indent (expected actual)
  (= (car expected) actual))

(defun make-error-msgs (expected actual)
  (cons
   (format "%s => %s (at line %d)" (nth 0 expected) (nth 1 expected) (nth 2 expected))
   actual))


(setq vc-handled-backends nil)
(cd (file-name-directory load-file-name))
(load (expand-file-name "./lib/test-util.el") nil t t)
(load (expand-file-name "../src/jsx-mode.el") nil t t)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
(setq calculate-indent-func 'jsx--calculate-indentation)

(if (run-test "jsx/indent" "jsx"
              :pre-func 'indent-test-pre-func
              :get-test-suite-func 'indent-test-get-test-suite
              :make-msg-func 'make-error-msgs
              :compare-func 'compare-indent)
    (kill-emacs)
  (kill-emacs 1))
