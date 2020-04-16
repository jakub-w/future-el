;;; future.el -- Futures using built-in threads -- -*- lexical-binding: t -*-

;; Copyright (C) 2020 Jakub Wojeciech

;; Author: Jakub Wojciech <jakub-w@riseup.net>
;; Created: 16 Apr 2020
;; Version: 0.0.1
;; Keywords: async
;; URL: https://github.com/jakub-w/future-el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file LICENSE.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The `future.el' package provides a simple implementation of futures
;; leveraging the Emacs' built-in threads.
;;
;; Keep in mind that if you want to speed up an operation by making it
;; concurrent some other implementation should be used. Emacs' threads
;; really work more like fibers than threads, i.e. they are not concurrent.
;; The internal implementation of threads in Emacs can change and hopefully
;; this package will be more useful in the future.
;;
;; To make a future use `make-future' function, providing it with a thunk,
;; just like you would when creating a thread:
;;
;; (make-future (lambda () (+ 10 20)))
;;
;; The returned value is a future object that will be needed for other
;; functions.
;; You can provide an optional callback to run after the future has finished.
;; It should be a function that takes one argument - value returned by the
;; thunk given as a first argument:
;;
;; (make-future (lambda () (+ 10 20))
;;              (lambda (result) (message "%s" result)))
;;
;; The callback will run in the same thread as the thunk.
;;
;; To get the value returned by a thunk without using a callback function use
;; `future-value' or `future-wait-for'.
;; `future-value' will block until the thunk has returned a value. Then return
;; it itself.
;; `future-wait-for' will block only for the specified amount of time or unil
;; the thunk returns. Its argument SECONDS can be an integer of a
;; floating-point number so you can give it a fraction of a second to wait.
;; It will return either the value or 'no-value symbol if the timeout was
;; reached.
;;
;; `future-has-value-p' can be used to check if the future already returned a
;; value without blocking.

;;; Code:

(defun futurep (object)
  "Check if OBJECT is a future."
  (and (vectorp object) (= (length object) 5) (eq (aref object 0) 'future)))

(defsubst future--thread (future)
  "Internal function. Don't use!"
  (aref future 1))

(defsubst future--set-thread (future thread)
  "Internal function. Don't use!"
  (aset future 1 thread))

(defsubst future--mutex (future)
  "Internal function. Don't use!"
  (aref future 2))

(defsubst future--cond-var (future)
  "Internal function. Don't use!"
  (aref future 3))

(defsubst future--has-value-p (future)
  "Internal function. Use `future-has-value-p' instead."
  (consp (future--value future)))

(defun future-has-value-p (future)
  "Check if the future already finished and set the value."
  (and (futurep future)
       (with-mutex (future--mutex future)
         (future--has-value-p future))))

(defsubst future--set-value (future value)
  "Internal function. Don't use!"
  (aset future 4 (list value)))

(defsubst future--value (future)
  "Internal function. Use `future-value' instead."
  (aref future 4))

(defun future-value (future)
  "Wait for the value and return it.

Blocks the current thread until the value is available."
  (unless (futurep future)
    (signal 'wrong-type-argument `(futurep ,future)))
  (with-mutex (future--mutex future)
    (while (not (future--has-value-p future))
      (condition-wait (future--cond-var future)))
    (car (future--value future))))

;;;###autoload
(defun make-future (thunk &optional callback)
  "Return a new future.

THUNK is a procedure that takes no arguments.

CALLBACK is a procedure that takes one argument - a value returned by the
future.  It is called after THUNK returns."
  (let* ((mutex (make-mutex))
         (f (vector 'future
                    nil
                    mutex
                    (make-condition-variable mutex)
                    'no-value)))
    (future--set-thread
     f
     (make-thread
      (lambda ()
        (let ((result (funcall thunk)))
          (with-mutex (future--mutex f)
            (future--set-value f result)
            (condition-notify (future--cond-var f)))
          (and callback (funcall callback (car (future--value f))))))))
    f))

(defun future-wait-for (future seconds)
  "Wait for SECONDS and return the value returned by the future or 'no-value
if the timeout was reached.

SECONDS is time in seconds. Can be fractional."
  (let ((elapsed 0.0)
        (interval (min 0.1 seconds)))
    (while (and (< elapsed seconds)
                (not (future-has-value-p future)))
      (sleep-for interval)
      (setq elapsed (+ interval elapsed)))
    (if (future-has-value-p future)
        (car (future--value future))
      'no-value)))

(provide 'future)
