;;; idate.el --- Configure read date -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/idate
;; Version: 0.1.0
;; Keywords: tools calendar
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Configure read date

;;; Code:




(defvar idate-current-time (decode-time (current-time)))

(declare-function calendar-forward-day "cal-move")
(declare-function calendar-absolute-from-gregorian "calendar")
(declare-function calendar-current-date "calendar")
(declare-function date-days-in-month "time-date")
(declare-function text-property-search-backward "text-property-search")

(defcustom idate-popup-calendar t
  "Non-nil means display a calendar when prompting for a date.
When nil, only the minibuffer will be available."
  :group 'org-time
  :type 'boolean)

(defvar idate-time-formats '((second . "%S")
                                    (minute . "%M")
                                    (hour . "%H")
                                    (day . "%d")
                                    (month . "%b")
                                    (year . "%Y")
                                    (dow . "%w")))


(defvar idate-time-elems '(second minute hour day month year dow dst
                                         utcoff))


(defcustom idate-rules-alist '((hour
                                       :separator ":")
                                      (minute)
                                      (day)
                                      (month
                                       :display "%B")
                                      (dow
                                       :display "%A")
                                      (year))
  "Rules for date parts."
  :group 'date
  :type `(alist
          :key-type
          (choice ,@(mapcar
                     (lambda (it)
                       (list 'const :tag (capitalize (symbol-name
                                                      it))
                             it))
                     (seq-take-while (lambda (it)
                                       (not (eq it 'dst)))
                                     idate-time-elems)))
          :value-type
          (plist :options
                 ((:separator (string :tag "Separator" " "))
                  (:display (string :tag "Display format"))))))

(defvar-local idate-rules nil)
(defun idate-inc-or-dec (step current-index min max)
  "Increase or decrease CURRENT-INDEX depending on STEP value, MIN and MAX."
  (let ((max (if (numberp max)
                 max
               (length max))))
    (cond ((< step 0)
           (if (< (+ step current-index)
                  min)
               max
             (+ step current-index)))
          ((> step 0)
           (if (> (+ step current-index) max)
               min
             (+ step current-index))))))



(defun idate-date-inc-or-dec (field &optional step)
  "Increment or decrement FIELD by STEP."
  (require 'time-date)
  (let ((idx (seq-position idate-time-elems field))
        (max (pcase field
               ('minute 59)
               ('hour 23)
               ('day (date-days-in-month
                      (nth (seq-position idate-time-elems 'year)
                           idate-current-time)
                      (nth (seq-position idate-time-elems 'month)
                           idate-current-time)))
               ('month 12)
               ('year 3000)
               ('dow 6)))
        (min (pcase field
               ('minute 0)
               ('hour 0)
               ('day 1)
               ('month 1)
               ('year 1000)
               ('dow 0)))
        (value)
        (new-value))
    (setq value (nth idx idate-current-time))
    (pcase field
      ((or 'day 'dow)
       (setq idate-current-time (idate-day-add-hours
                                        (if (> step 0) 24 -24))))
      ('month
       (setq idate-current-time
             (idate-day-add-hours (+
                                          (* (if (> step 0) 24 -24)
                                             30)))))
      (_
       (setq new-value (idate-inc-or-dec step value min max))
       (setf (nth idx idate-current-time) new-value)))
    (idate-rerender)
    (idate-calendar-eval)))

(defun idate-day-add-hours (hours)
  "Return decoded `idate-current-time' with HOURS added."
  (let ((time (time-to-seconds (apply
                                #'encode-time
                                idate-current-time))))
    (decode-time (seconds-to-time (+ time (* hours 3600))))))


(defun idate-set-field-value (field-name new-value)
  "Update date and rerender FIELD-NAME with NEW-VALUE."
  (let ((idx (seq-position idate-time-elems field-name)))
    (setf (nth idx
               idate-current-time)
          new-value)
    (setq idate-current-time
          (decode-time (encode-time
                        idate-current-time)))
    (idate-rerender)))

(defun idate-time-field-valid-p (field-name value)
  "Return t if VALUE of FIELD-NAME is valid."
  (let ((max-val (pcase field-name
                   ('minute 59)
                   ('hour 23)
                   ('day (date-days-in-month
                          (nth (seq-position idate-time-elems 'year)
                               idate-current-time)
                          (nth (seq-position idate-time-elems 'month)
                               idate-current-time)))
                   ('month 12)
                   ('year 3000)
                   ('dow 6)))
        (min-val (pcase field-name
                   ('minute 0)
                   ('hour 0)
                   ('day 1)
                   ('month 1)
                   ('dow 0))))
    (cond ((and max-val min-val)
           (and (<= value max-val)
                (>= value min-val)))
          ((and max-val)
           (<= value max-val))
          ((and min-val)
           (>= value min-val))
          (t t))))

(defun idate-jump-to-field (field-name)
  "Jump to FIELD-NAME."
  (require 'text-property-search)
  (let ((field (idate-field-name-at-point)))
    (unless (eq field-name
                field)
      (when (or
             (text-property-search-forward 'field-name field-name
                                           t)
             (text-property-search-backward 'field-name
                                            field-name
                                            t))
        (when-let ((bounds (idate-field-bounds-at-point)))
          (goto-char (car bounds)))))))

(defun idate-goto-hours ()
  "Jump to hours in minubuffer."
  (interactive)
  (idate-jump-to-field 'hour))


(defun idate-goto-minutes ()
  "Jump to minutes in minubuffer."
  (interactive)
  (idate-jump-to-field 'minute))


(defun idate-goto-day ()
  "Jump to day in minubuffer."
  (interactive)
  (idate-jump-to-field 'day))


(defun idate-goto-year ()
  "Jump to year in minubuffer."
  (interactive)
  (idate-jump-to-field 'year))


(defun idate-goto-month ()
  "Jump to month in minubuffer."
  (interactive)
  (idate-jump-to-field 'month))


(defun idate-done ()
  "Throw done with encoded `idate-current-time'."
  (interactive)
  (throw 'done
         (apply
          #'encode-time
          idate-current-time)))


(defun idate-toggle-with-time ()
  "Throw done with encoded `idate-current-time'."
  (interactive)
  (setq idate-rules
        (if (= (length idate-rules)
               (length idate-rules-alist))
            (seq-remove
             (lambda (it)
               (memq
                (car
                 it)
                '(hour
                  minute
                  second)))
             (seq-copy
              idate-rules-alist))
          (seq-copy
           idate-rules-alist)))
  (idate-rerender))


(defun idate-inc-day ()
  "Increment current field."
  (interactive)
  (save-excursion
    (idate-goto-day)
    (idate-inc-current)))


(defun idate-dec-day ()
  "Increment current field."
  (interactive)
  (save-excursion
    (idate-goto-day)
    (idate-dec-current)))


(defun idate-inc-month ()
  "Increment current field."
  (interactive)
  (save-excursion
    (idate-goto-month)
    (idate-inc-current)))


(defun idate-dec-month ()
  "Increment current field."
  (interactive)
  (save-excursion
    (idate-goto-month)
    (idate-dec-current)))


(defun idate-inc-current ()
  "Increment current field."
  (interactive)
  (when-let* ((bounds (idate-get-prop-bounds 'field-name))
              (field-name (save-excursion
                            (goto-char (car bounds))
                            (get-text-property (point) 'field-name))))
    (idate-date-inc-or-dec field-name  1)))


(defun idate-dec-current ()
  "Decrement current field."
  (interactive)
  (when-let* ((bounds (idate-get-prop-bounds 'field-name))
              (field-name (save-excursion
                            (goto-char (car bounds))
                            (get-text-property (point) 'field-name))))
    (idate-date-inc-or-dec field-name  -1)))

(defvar idate-minubuffer-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "m") 'idate-goto-minutes)
    (define-key map (kbd "h") 'idate-goto-hours)
    (define-key map (kbd "d") 'idate-goto-day)
    (define-key map (kbd "y") 'idate-goto-year)
    (define-key map (kbd "M") 'idate-goto-month)
    (define-key map (kbd "<up>") 'idate-inc-current)
    (define-key map (kbd "<down>") 'idate-dec-current)
    (define-key map (kbd "S-<up>") 'idate-inc-day)
    (define-key map (kbd "S-<down>") 'idate-dec-day)
    (define-key map (kbd "S-<right>") 'idate-inc-month)
    (define-key map (kbd "S-<left>") 'idate-dec-month)
    (define-key map (kbd "f") 'idate-inc-current)
    (define-key map (kbd "b") 'idate-dec-current)
    (define-key map (kbd "<left>")
                'idate-prev-field)
    (define-key map (kbd "TAB")
                'idate-next-field)
    (define-key map (kbd "<backtab>")
                'idate-prev-field)
    (define-key map (kbd "<right>")
                'idate-next-field)
    (define-key map (kbd "RET") 'idate-done)
    (define-key map (kbd "SPC") 'idate-next-field)
    (define-key map (kbd "0") 'idate-edit-field-at-point)
    (define-key map (kbd "1") 'idate-edit-field-at-point)
    (define-key map (kbd "2") 'idate-edit-field-at-point)
    (define-key map (kbd "3") 'idate-edit-field-at-point)
    (define-key map (kbd "4") 'idate-edit-field-at-point)
    (define-key map (kbd "5") 'idate-edit-field-at-point)
    (define-key map (kbd "6") 'idate-edit-field-at-point)
    (define-key map (kbd "7") 'idate-edit-field-at-point)
    (define-key map (kbd "8") 'idate-edit-field-at-point)
    (define-key map (kbd "9") 'idate-edit-field-at-point)
    (define-key map (kbd "C->") 'idate-toggle-with-time)
    (define-key map (kbd "C-.") 'idate-toggle-with-time)
    (define-key map [remap (delete-backward-char)]
                'left-char)
    map))

(defun idate-field-bounds-at-point ()
  "Return current date field-name at point."
  (idate-get-prop-bounds 'field-name))

(defun idate-get-prop-bounds (prop &optional pos)
  "Return property boundaries for PROP at POS."
  (unless pos (setq pos (point)))
  (goto-char pos)
  (if (get-text-property (point) prop)
      (when-let* ((end (next-single-char-property-change (point) prop))
                  (beg (save-excursion
                         (goto-char end)
                         (previous-single-char-property-change
                          (point) prop))))
        (cons beg end))
    (when-let* ((beg (previous-single-char-property-change (point) prop))
                (end (save-excursion
                       (goto-char beg)
                       (next-single-char-property-change (point) prop))))
      (cons beg end))))



(defun idate-next-field ()
  "Goto to next field-name in minibuffer."
  (interactive)
  (when-let ((bounds (idate-field-bounds-at-point)))
    (goto-char (cdr bounds))
    (when-let ((found (next-single-char-property-change (point) 'field-name)))
      (goto-char found))))


(defun idate-prev-field ()
  "Goto to previous field-name in minibuffer."
  (interactive)
  (when-let ((bounds (idate-field-bounds-at-point)))
    (goto-char (car bounds))
    (text-property-search-backward 'field-name)))

(defun idate-field-name-at-point ()
  "Return current field name at point in minibuffer."
  (when-let ((bounds (idate-get-prop-bounds 'field-name)))
    (get-text-property (car bounds) 'field-name)))


(defun idate-edit-field-at-point ()
  "Edit current date field at point."
  (interactive)
  (when-let* ((bounds (idate-get-prop-bounds 'field-name))
              (field-name (save-excursion
                            (goto-char (car bounds))
                            (get-text-property (point) 'field-name))))
    (let* ((descr (key-description (this-command-keys)))
           (beg (car bounds))
           (end (cdr bounds))
           (field-value (buffer-substring-no-properties beg end))
           (value-before (buffer-substring-no-properties
                          beg
                          (point)))
           (value-after (buffer-substring-no-properties (point)
                                                        end))
           (new-value))
      (setq new-value (cond ((string-empty-p value-after)
                             (concat
                              (substring value-before 0
                                         (- 1 (length field-value)))
                              descr))
                            (t (concat value-before descr
                                       (substring value-after 1)))))
      (unless (string-match-p "[^0-9]" new-value)
        (when (idate-time-field-valid-p
               field-name
               (string-to-number new-value))
          (idate-set-field-value
           field-name (string-to-number
                       new-value))
          (if (string-empty-p value-after)
              (idate-next-field)
            (forward-char 1)))
        (idate-calendar-eval)))))

(defvar idate-ovl (make-overlay 1 1))
(overlay-put idate-ovl 'face 'org-date-selected)
(delete-overlay idate-ovl)

(defun idate--eval-in-calendar (form)
  "Eval FORM in the calendar window and return to current window."
  (let ((sf (selected-frame))
        (sw (selected-window)))
    (select-window (get-buffer-window "*Calendar*" t))
    (eval form t)
    (move-overlay idate-ovl (1- (point))
                  (1+ (point))
                  (current-buffer))
    (select-window sw)
    (select-frame-set-input-focus sf)))

(defun idate-calendar-eval ()
  "Sync calendar with `idate-current-time'."
  (require 'calendar)
  (require 'cal-move)
  (save-excursion
    (calendar)
    (save-window-excursion
      (calendar)
      (idate--eval-in-calendar '(setq cursor-type nil))
      (unwind-protect
          (progn
            (calendar-forward-day (-
                                   (time-to-days
                                    (apply #'encode-time
                                           idate-current-time))
                                   (calendar-absolute-from-gregorian
                                    (calendar-current-date))))
            (idate--eval-in-calendar nil))
        (bury-buffer "*Calendar*"))))
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(defun idate-rerender ()
  "Render `idate-current-time' in minibuffer."
  (when (minibufferp)
    (let ((inhibit-read-only t)
          (pos (point)))
      (delete-region (minibuffer-prompt-end)
                     (point-max))
      (insert (idate-time-render-value
               (encode-time idate-current-time)))
      (goto-char pos)
      (add-text-properties (point-min)
                           (point-max) '(read-only t)))))

(defun idate-time-render-value (time)
  "Render encoded TIME as string."
  (let* ((decoded (decode-time time))
         (encoded (apply
                   #'encode-time
                   decoded)))
    (string-trim
     (mapconcat (lambda (it)
                  (let* ((field-name (car it))
                         (props (cdr it))
                         (keymap (copy-keymap
                                  idate-minubuffer-keymap))
                         (format-str (cdr (assoc field-name
                                                 idate-time-formats)))
                         (extra-props
                          (when (plist-get props :display)
                            (list 'display (format-time-string
                                            (seq-copy (plist-get props
                                                                 :display))
                                            encoded)
                                  'invisible t)))
                         (value (format-time-string
                                 format-str
                                 encoded)))
                    (concat (apply #'propertize
                                   value
                                   (append
                                    `(field-name
                                      ,field-name
                                      keymap
                                      ,keymap
                                      read-only t)
                                    extra-props))
                            (or
                             (plist-get props :separator)
                             " "))))
                idate-rules))))

(defun idate-read-and-format (&optional prompt default-value without-time
                                        format-str)
  "Read date with PROMPT and return formatted with FORMAT-STR result.
Optional argument DEFAULT-VALUE should be encoded time.
If WITHOUT-TIME don't display time."
  (format-time-string
   (or format-str (let ((default-format-cell
                         (if
                             (bound-and-true-p
                              org-time-stamp-formats)
                             org-time-stamp-formats
                           '("%Y-%m-%d %a"
                             .
                             "%Y-%m-%d %a %H:%M"))))
                    (if without-time
                        (car default-format-cell)
                      (cdr default-format-cell))))
   (idate-read prompt default-value
               without-time)))
;;;###autoload
(defun idate-read (&optional prompt default-value without-time)
  "Read date in minibuffer with PROMPT and return encoded result.
Optional argument DEFAULT-VALUE should be encoded time.
If WITHOUT-TIME don't display time."
  (setq idate-current-time
        (decode-time (or default-value (current-time))))
  (catch 'done (minibuffer-with-setup-hook
                   (lambda ()
                     (when (active-minibuffer-window)
                       (let* ((map
                               (copy-keymap
                                idate-minubuffer-keymap)))
                         (set-keymap-parent map
                                            minibuffer-local-map)
                         (use-local-map map)
                         (setq-local
                          idate-rules
                          (if
                              without-time
                              (seq-remove
                               (lambda (it)
                                 (memq
                                  (car
                                   it)
                                  '(hour
                                    minute
                                    second)))
                               idate-rules-alist)
                            (seq-copy
                             idate-rules-alist)))
                         (let ((inhibit-read-only
                                t))
                           (idate-rerender)
                           (add-text-properties
                            (point-min)
                            (point-max)
                            '(read-only
                              t)))
                         (when idate-popup-calendar
                           (idate-calendar-eval)))))
                 (read-from-minibuffer (or
                                        prompt
                                        "Date: ")))))

(defun idate-insert-time-stamp (&optional without-hm inactive pre post extra)
  "Read and insert date as org timestamp.
See `format-time-string' for the format of TIME.
WITHOUT-HM inhibit the stamp format that includes the time of the day.
INACTIVE means use square brackets instead of angular ones, so that the
stamp will not contribute to the agenda.
PRE and POST are optional strings to be inserted before and after the
stamp.
EXTRA is last argument to pass in `org-insert-time-stamp'.
The command returns the inserted time stamp."
  (when (fboundp 'org-insert-time-stamp)
    (org-insert-time-stamp (idate-read "Timestamp: " nil without-hm)
                           (not without-hm) inactive pre post extra)))

;;;###autoload
(defun idate-insert-org-time-stamp ()
  "Read and insert date as org timestamp.
See `format-time-string' for the format of TIME.
WITHOUT-HM inhibit the stamp format that includes the time of the day.
INACTIVE means use square brackets instead of angular ones, so that the
stamp will not contribute to the agenda.
PRE and POST are optional strings to be inserted before and after the
stamp.
EXTRA is last argument to pass in `org-insert-time-stamp'.
The command returns the inserted time stamp."
  (interactive)
  (when (fboundp 'org-insert-time-stamp)
    (let ((without-hm (not (yes-or-no-p "With hh:mm?"))))
      (idate-insert-time-stamp without-hm
                                      nil nil nil nil))))

(provide 'idate)
;;; idate.el ends here
