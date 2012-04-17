;; Copyright (C) 2012 yewton

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

(require 'parse-time)
(defvar howm-notify-schedule nil)
(defvar howm-notify-schedule-list nil)
(defvar howm-notify-schedule-timer-handler nil)
(defvar howm-notify-schedule-check-interval 60)
(defvar howm-notify-schedule-function-list
  '(howm-notify-schedule-in-echo-area))
(defvar howm-notify-before-min 5)
(defvar howm-notify-title "予定の通知")

(defun howm-notify-schedule-get-time-string (str)
  (if (string-match
         "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\s-+[0-9]\\{2\\}:[0-9]\\{2\\}"
         str)
    (match-string 0 str)
    ""))

(defun howm-notify-schedule-trim (str)
  (let ((ms (if (string-match "^\\s-*\\(\\S-.*\\)" str)
                (match-string 1 str)
              str)))
    (if (string-match "\\(.*\\S-\\)\\s-*$" ms)
        (match-string 1 ms)
      ms)))

(defun howm-get-well-formed-schedule-list ()
  (let ((schedule-list
         (howm-schedule-sort-items
          (howm-list-reminder-internal howm-schedule-types)))
        (result-list ()))
    (when schedule-list
      (dolist (item schedule-list (reverse result-list))
        (let* ((body(second item))
               (time-string (howm-notify-schedule-get-time-string body))
               (time (parse-time-string time-string))
               (title (howm-notify-schedule-trim
                       (nth 5 (howm-todo-parse-string body))))
               (sec (first time))
               (min (second time))
               (notify-time
                (when (and sec min)
                  (append
                   (list sec (- min howm-notify-before-min))
                   (cddr time))))
               (notify-time-encoded
                (when (and sec min)
                  (apply 'encode-time time))))
          (when (and sec min)
            (when (time-less-p (current-time) notify-time-encoded)
              (add-to-list 'result-list
                           (list notify-time title body time)))))))))

(defun howm-notify-schedule-rescan ()
  (interactive)
  (if (setq howm-notify-schedule-list (howm-get-well-formed-schedule-list))
    (message "howm schedule re-scanned.")))

(defun howm-notify-schedule-start ()
  (interactive)
  (when (null howm-notify-schedule-list)
    (howm-notify-schedule-rescan))
  (if (timerp howm-notify-schedule-timer-handler)
      (message "howm notify schedule already started.")
    (setq howm-notify-schedule-timer-handler
          (run-at-time t howm-notify-schedule-check-interval
                       'howm-notify-schedule-check))
    (message "howm notify schedule started.")))

(defun howm-notify-schedule-stop ()
  (interactive)
  (if (timerp howm-notify-schedule-timer-handler)
    (progn
      (cancel-timer howm-notify-schedule-timer-handler)
      (setq howm-notify-schedule-timer-handler nil)
      (message "howm notify schedule stopped."))
    (message "howm notify schedule not started.")))

(defun howm-notify-schedule-in-echo-area (title time)
  (message "Reminder: %s %s"
           (format-time-string
            "%H:%M"
            (apply 'encode-time time))
           title))

(defun howm-notify-schedule-check ()
  (let* ((result ())
        (rest-notify-list ())
        (notify-list ()))
    (dolist (item howm-notify-schedule-list)
      (if (time-less-p
           (time-add
            (apply 'encode-time (first item))
            (seconds-to-time (- (/ howm-notify-schedule-check-interval 2))))
           (current-time))
          (add-to-list 'notify-list item)
        (add-to-list 'rest-notify-list item)))
    (dolist (item notify-list result)
      (when item
        (let ((notify-time (first item))
              (title (second item))
              (body (third item))
              (time (fourth item)))
          (dolist (func howm-notify-schedule-function-list)
            (funcall func title time)))))
    (setq howm-notify-schedule-list rest-notify-list)))

(provide 'howm-notify-schedule)
