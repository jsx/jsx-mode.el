;;; test-util.el --- utility for a unit test

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

(require 'cl)

(defmacro push-end (x place)
  `(if ,place
       (nconc ,place (list ,x))
     (setq ,place (list ,x))))

(defun* join (string-list &optional (delim ""))
  (mapconcat 'identity string-list delim))

(defun make-error-msg (func expected actual)
  (let ((msg (funcall func expected actual)))
    (format
     "\texpected: %s\n\tactual: %s"
     (car msg) (cdr msg))))


(defun* run-test (dir
                  &optional ext
                  &key get-test-suite-func make-msg-func
                       pre-func (compare-func 'equal))
  (if ext
      (setq ext (format "\\.%s\\'" ext)))
  (let* ((cnt 0)
         (passed-cnt 0)
         (failed-cnt 0)
         (msg "")
         (start-time (current-time-string))
         (files (directory-files dir nil ext))
         (total-cnt (length files)))
    (unwind-protect
        (dolist (file files)
          (with-current-buffer (find-file-noselect (concat dir "/" file))
            (if pre-func
                (funcall pre-func))
            (let* ((test-suite (funcall get-test-suite-func))
                   (expecteds (car test-suite))
                   (actuals (cdr test-suite))
                   (len (max (length expecteds) (length actuals)))
                   (errors '()))
              (if (= len 0)
                  (decf total-cnt)
                (loop for i from 0 to (1- len)
                      for expected = (nth i expecteds)
                      for actual = (nth i actuals)
                      do (condition-case ex
                             (assert (funcall compare-func expected actual))
                           (error
                            (push-end (make-error-msg make-msg-func expected actual) errors))))
                (if (= (length errors) 0)
                    (incf passed-cnt)
                  (setq msg (format "%sF %s:\n%s\n\n" msg file (join errors "\n\n")))
                  (incf failed-cnt))))))
      (princ
       (concat
        "Passed: " (format "%d\n" passed-cnt)
        "Failed: " (format "%d\n" failed-cnt)
        "Total: " (format "%d/%d\n" (+ passed-cnt failed-cnt) total-cnt)
        "\n"
        "Started at:  " start-time "\n"
        "Finished at: " (current-time-string) "\n"
        "\n"
        msg)))
    (string= msg "")))
