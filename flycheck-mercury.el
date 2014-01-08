;;; flycheck-mercury.el --- flycheck checker for Mercury using mmc

;; Copyright (c) 2014 Matthias Güdemann <matthias.gudemann@gmail.com>
;;
;; Author: Matthias Güdemann <matthias.gudemann@gmail.com>
;; URL: https://github.com/flycheck/flycheck-mercury
;; Keywords: convenience languages tools
;; Version: DEV
;; Package-Requires: ((flycheck "0.15"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; adds Mercury checker to flycheck using the Melbourne Mercury Compiler

;;; Code:

(require 'flycheck)

(add-to-list 'flycheck-checkers
             'mercury-mmc)

(defvar flycheck-mmc-message-width 1000
  "Max width to pass to mmc --max-error-line-width.")

(eval-and-compile
  (defun flycheck-mmc-remove-redundant-errors (output)
    "Removes redundant errors without line number from OUTPUT.

This function removes errors from the list of message lines in
output where the message is prefixed with
'mercury_compile:'. These errors represent generated interfaces
files etc. that cannot be located and do not have a line number
associated. The errors appear again later when the corresponding
types etc. are used. "
    (-remove #'(lambda (zeile)
                 (s-starts-with? "mercury_compile:" zeile)) output)))

(eval-and-compile
  (defun flycheck-mmc-compute-line-desc-pairs (output)
    "Compute list of (linenumber . part of message) from OUTPUT.

OUTPUT is the raw mercury warning / error message output of the format:
'filename ':' linenumber ':' errormessage'."
     (mapcar #'(lambda (num-desc)
                 (cons (string-to-number (car num-desc))
                       (-reduce #'(lambda (zeile rest)
                                    (concat zeile ":" rest))
                                (cdr num-desc))))
             (-remove #'(lambda (x) (eq x nil))
                      (mapcar #'(lambda (zeile)
                                  (cdr (split-string zeile ":")))
                              (flycheck-mmc-remove-redundant-errors
                               (split-string output "\n")))))))

(eval-and-compile
  (defun flycheck-mmc-compute-line-desc-maps (line-desc-pairs)
    "Compute map of line numbers to messages from LINE-DESC-PAIRS.

The input list of pairs of linenumbers and messages is
transformed to a list of lists where each sublist is a list of
cons cells containing the linenumber and message part.  The
result is grouped for line numbers."
    (mapcar #'(lambda (elem)
                (-filter #'(lambda (x)
                             (eq (car x) elem)) line-desc-pairs))
            (delete-dups (mapcar #'(lambda (line-desc)
                                     (car line-desc)) line-desc-pairs)))))

(eval-and-compile
  (defun flycheck-mmc-compute-final-list (line-desc-maps)
    "Compute alist from LINE-DESC-MAPS.

Computes an alist from the line numbers to the concatenation of
messages for that line number."
    (mapcar #'(lambda (x)
                (cons (car x) (split-string (cdr x) "\\. ")))
            (mapcar #'(lambda (entry)
                        (cons (car (car entry))
                              (-reduce #'(lambda (prefix rest)
                                           (concat prefix rest "\n"))
                                       (cons "" (mapcar #'cdr entry)))))
                    line-desc-maps))))

(eval-and-compile
  (defun flycheck-mmc-compute-flycheck-errors (final-list)
    "Compute the list fo flycheck-error objects from FINAL-LIST."
    (mapcar #'(lambda (x)
                (flycheck-error-new :line (car x)
                                    :message (cadr x)
                                    :level (if (string-match "rror" (cadr x))
                                               'error
                                               'warning)))
            final-list)))

(eval-and-compile
  (defun flycheck-mmc-error-parser (output checker buffer)
    "Parse the OUTPUT buffer, ignore CHECKER and BUFFER.

Parses the Mercury warning / error output, provides interface
for :error-parser functions for flycheck."
    (let ((line-desc-pairs (flycheck-mmc-compute-line-desc-pairs output)))
      (let ((line-desc-maps (flycheck-mmc-compute-line-desc-maps line-desc-pairs)))
        (let ((final-list (flycheck-mmc-compute-final-list line-desc-maps)))
          (flycheck-mmc-compute-flycheck-errors final-list))))))

(flycheck-define-checker mercury-mmc
  "A Mercury syntax and type checker using mmc.

See URL `http://mercurylang.org/'."
  :command ("mmc"
            "-E"
            "-e"
            "--max-error-line-width" (eval (number-to-string
                                            flycheck-mmc-message-width))
            source)
  :error-parser flycheck-mmc-error-parser
  :modes (mercury-mode prolog-mode))


(provide 'flycheck-mercury)

;; Local Variables:
;; coding: utf-8
;; End:

;;; flycheck-mercury.el ends here
