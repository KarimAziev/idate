;;; idate.el --- Configure read date -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/idate
;; Version: 0.1.0
;; Keywords: tools calendar
;; Package-Requires: ((emacs "29.1") (org "9.6.10"))
;; SPDX-License-Identifier: GPL-3.0-or-later

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




(require 'calendar)
(require 'org)

(declare-function date-days-in-month "time-date")
(declare-function text-property-search-backward "text-property-search")

(defvar idate-current-time nil
  "Timestamp of (SEC MINUTE HOUR DAY MONTH YEAR DOW DST UTCOFF).")

(defcustom idate-popup-calendar t
  "Whether to show a calendar when editing dates.

Determines whether a popup calendar is displayed when editing date fields in an
interactive date editing environment. When non-nil, the popup calendar is shown
to assist with date selection. When nil, the popup calendar is not displayed.

The default value is t, enabling the popup calendar by default.

To change this behavior, set the value to nil. This can be done programmatically
by setting the variable to nil in Lisp code, or interactively through the
customization interface for date and time related settings.

Changing the value of this variable while editing a date field will take effect
immediately, either showing or hiding the popup calendar based on the new value."
  :group 'org-time
  :type 'boolean)

(defvar idate-time-formats
  '((second . "%S")
    (minute . "%M")
    (hour . "%H")
    (day . "%d")
    (month . "%m")
    (year . "%Y")
    (dow . "%w"))
  "Alist mapping time units to strftime format strings.")

(defvar idate-time-elems '(second minute hour day month year dow dst
                           utcoff)
  "Ordered list of date-time elements.

In includes second, minute, hour, day, month, year, day of week, DST, UTC
offset.")

(defcustom idate-rules-alist '((hour
                                :separator ":")
                               (minute)
                               (day)
                               (month :display "%b")
                               (dow
                                :display "%A")
                               (year))
  "Alist mapping date/time components to display and separator rules.

An association list defining the rules for interactive date components and their
display formats. Each element of the list is a cons cell where the car is a
symbol representing a date or time component, and the cdr is a property list
specifying options for that component.

Supported components are `day', `month', `dow' (day of the week), `year',
`hour', and `minute'.

The property list can contain the following keys:

- :separator, with a string value, defines the string to use to separate this
component from the next when displaying the date. The default separator is a
space (\" \").

- :display, with a string value, specifies the format to use when displaying the
component. The format should be a string compatible with the
`format-time-string' function. If not provided, the default display format for
the component is used.

For example, to display the month as a three-letter abbreviation followed by a
period (e.g., \"Jan.\"), set the :display property for the `month' component to
\"%b.\". To separate the hour and minute components with a hyphen instead of the
default colon, set the :separator property for the `hour' component to \"-\".

To modify the display of interactive date components, customize this list
according to the desired output format."
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
  "Increment or decrement index within bounds.

Argument STEP is an integer indicating the increment or decrement step.

Argument CURRENT-INDEX is the current index as an integer.

Argument MIN is the minimum index value as an integer.

Argument MAX is either the maximum index value as an integer or a list whose
length determines the maximum index value."
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
  "Increment or decrement date fields by a given step.

Argument FIELD is a symbol representing the date or time field to be incremented
or decremented.

Optional argument STEP is an integer specifying the amount to increment or
decrement the FIELD by; it defaults to 1."
  (require 'time-date)
  (let ((idx (seq-position idate-time-elems field))
        (max
         (pcase field
           ('minute 59)
           ('hour 23)
           ('day (date-days-in-month
                  (nth (seq-position idate-time-elems 'year)
                       idate-current-time)
                  (nth (seq-position idate-time-elems 'month)
                       idate-current-time)))
           ('month 12)
           ('year 2999)
           ('dow 6)))
        (min
         (pcase field
           ('minute 0)
           ('hour 0)
           ('day 1)
           ('month 1)
           ('year 1970)
           ('dow 0)))
        (value)
        (new-value))
    (setq value (nth idx idate-current-time))
    (pcase field
      ((or 'day 'dow)
       (setq idate-current-time (idate-day-add-hours
                                 (if (> step 0) 24 -24)
                                 idate-current-time)))
      ('month
       (let* ((day  (nth (seq-position idate-time-elems 'day)
                         idate-current-time))
              (new-val (idate-day-add-hours (+
                                             (* (if (> step 0) 24 -24)
                                                (date-days-in-month
                                                 (nth
                                                  (seq-position idate-time-elems
                                                                'year)
                                                  idate-current-time)
                                                 (nth (seq-position
                                                       idate-time-elems
                                                       'month)
                                                      idate-current-time))))
                                            idate-current-time))
              (new-day (nth (seq-position idate-time-elems 'day)
                            new-val))
              (max-day (date-days-in-month (nth
                                            (seq-position idate-time-elems
                                                          'year)
                                            new-val)
                                           (nth (seq-position idate-time-elems
                                                              'month)
                                                new-val))))
         (when (and (> max-day day)
                    (not (= new-day day)))
           (setf (nth (seq-position idate-time-elems 'day) new-val) day))
         (setq idate-current-time
               new-val)))
      (_
       (setq new-value (idate-inc-or-dec step value min max))
       (setf (nth idx idate-current-time) new-value)))
    (idate-rerender)))

(defun idate-day-add-hours (hours decoded-time)
  "Add HOURS to DECODED-TIME and return the result.

Argument HOURS is the number of hours to add to the decoded time.

Argument DECODED-TIME is a list representing a decoded time, as returned by
`decode-time'."
  (let ((time (time-to-seconds (apply
                                #'encode-time
                                decoded-time))))
    (decode-time (seconds-to-time (+ time (* hours 3600))))))

(defun idate-set-fields-value (alist)
  "Set specified field values in `idate-current-time' alist.

Argument ALIST is a list of cons cells where each cell's car is a field name and
cdr is the corresponding value to set."
  (dolist (it alist)
    (let ((field-name (car it))
          (field-value (if (listp (cdr it))
                           (nth 1 it)
                         (cdr it))))
      (let ((idx (seq-position idate-time-elems field-name)))
        (setf (nth idx idate-current-time) field-value))))
  idate-current-time)

(defun idate-set-field-value (field-name new-value)
  "Set field FIELD-NAME in `idate-current-time' to NEW-VALUE.

Argument FIELD-NAME is a symbol representing the field to set; it must be one of
`second', `minute', `hour', `day', `month', `year', `dow', `dst', or `utcoff'.

Argument NEW-VALUE is the new value to set for the specified field; it should be
an integer or a boolean for `dst'."
  (let ((idx (seq-position idate-time-elems field-name)))
    (setf (nth idx
               idate-current-time)
          new-value)
    (setq idate-current-time
          (decode-time (encode-time
                        idate-current-time)))))

(defun idate-get-field-valid-value (field-name value)
  "Ensure VALUE is within FIELD-NAME's valid range.

Argument FIELD-NAME is a symbol representing the date field, such as `minute',
`hour', \\=`day', `month', `year', or `dow'.

Argument VALUE is an integer representing the value to validate against the
field specified by FIELD-NAME."
  (let ((max-val
         (pcase field-name
           ('minute 59)
           ('hour 23)
           ('day (date-days-in-month
                  (nth (seq-position idate-time-elems 'year)
                       idate-current-time)
                  (nth (seq-position idate-time-elems 'month)
                       idate-current-time)))
           ('month 12)
           ('year 2999)
           ('dow 6)))
        (min-val
         (pcase field-name
           ('minute 0)
           ('year 1970)
           ('hour 0)
           ('day 1)
           ('month 1)
           ('dow 0)))
        (valid))
    (setq valid (cond ((and max-val min-val)
                       (and (<= value max-val)
                            (>= value min-val)))
                      ((and max-val)
                       (<= value max-val))
                      ((and min-val)
                       (>= value min-val))
                      (t t)))
    (if valid
        value
      (if (and min-val max-val)
          (if (> value max-val)
              max-val
            min-val)
        (or min-val max-val)))))

(defun idate-jump-to-field (field-name)
  "Jump to a specified field in the minibuffer.

Argument FIELD-NAME is the name of the field to jump to."
  (require 'text-property-search)
  (let ((field (idate-field-name-at-point)))
    (unless (eq field-name
                field)
      (goto-char (minibuffer-prompt-end))
      (text-property-search-forward 'field-name field-name
                                    t)
      (when-let ((bounds (idate-field-bounds-at-point)))
        (goto-char (car bounds))))))

(defun idate-goto-hours ()
  "Jump to the `hour' field in a date input."
  (interactive)
  (idate-jump-to-field 'hour))

(defun idate-goto-minutes ()
  "Jump to the `minute' field in a date input."
  (interactive)
  (idate-jump-to-field 'minute))

(defun idate-goto-day ()
  "Jump to the `day' field in an interactive date prompt."
  (interactive)
  (idate-jump-to-field 'day))

(defun idate-goto-year ()
  "Jump to the `year' field in an interactive date prompt."
  (interactive)
  (idate-jump-to-field 'year))

(defun idate-goto-month ()
  "Jump to the month field in an interactive date prompt."
  (interactive)
  (idate-jump-to-field 'month))

(defun idate-done ()
  "Throw \\='done signal with encoded `idate-current-time'."
  (interactive)
  (throw 'done
         (apply
          #'encode-time
          idate-current-time)))

(defvar idate--org-with-time t
  "Whether to include time with date in insertion.")

(defun idate-toggle-with-time ()
  "Toggle date display to include or exclude time components."
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
  (setq idate--org-with-time
        (= (length idate-rules)
           (length idate-rules-alist)))
  (idate-rerender))

(defun idate-today ()
  "Set `idate-current-time' to the current time and update display."
  (interactive)
  (setq idate-current-time (decode-time (current-time)))
  (idate-rerender))

(defun idate-inc-day ()
  "Increment the day in the date field by one."
  (interactive)
  (save-excursion
    (idate-goto-day)
    (idate-inc-current)))

(defun idate-dec-day ()
  "Decrement the day in the current date field."
  (interactive)
  (save-excursion
    (idate-goto-day)
    (idate-dec-current)))

(defun idate-inc-month ()
  "Increment the current month in the date field by one."
  (interactive)
  (save-excursion
    (idate-goto-month)
    (idate-inc-current)))

(defun idate-dec-month ()
  "Decrement the month in the current date field."
  (interactive)
  (save-excursion
    (idate-goto-month)
    (idate-dec-current)))

(defun idate-dec-year ()
  "Decrement the year in the current date field."
  (interactive)
  (save-excursion
    (idate-goto-year)
    (idate-dec-current)))

(defun idate-inc-year ()
  "Increment the year in the date field by one."
  (interactive)
  (save-excursion
    (idate-goto-year)
    (idate-inc-current)))

(defun idate-inc-current ()
  "Increment the date field at point by one."
  (interactive)
  (when-let* ((bounds (idate-get-prop-bounds 'field-name))
              (field-name (save-excursion
                            (goto-char (car bounds))
                            (get-text-property (point) 'field-name))))
    (idate-date-inc-or-dec field-name  1)))

(defun idate-dec-current ()
  "Decrement the current date field value."
  (interactive)
  (when-let* ((bounds (idate-get-prop-bounds 'field-name))
              (field-name (save-excursion
                            (goto-char (car bounds))
                            (get-text-property (point) 'field-name))))
    (idate-date-inc-or-dec field-name  -1)))

(defvar idate-minubuffer-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "m") #'idate-goto-minutes)
    (define-key map (kbd "h") #'idate-goto-hours)
    (define-key map (kbd "d") #'idate-goto-day)
    (define-key map (kbd "y") #'idate-goto-year)
    (define-key map (kbd "M") #'idate-goto-month)
    (define-key map (kbd "<up>") #'idate-inc-current)
    (define-key map (kbd "<down>") #'idate-dec-current)
    (define-key map (kbd "S-<right>") #'idate-inc-day)
    (define-key map (kbd "S-<left>") #'idate-dec-day)
    (define-key map (kbd "M-<right>") #'idate-inc-month)
    (define-key map (kbd "M-<left>") #'idate-dec-month)
    (define-key map (kbd "M-<up>") #'idate-inc-month)
    (define-key map (kbd "M-<down>") #'idate-dec-month)
    (define-key map (kbd "M-S-<up>") #'idate-inc-year)
    (define-key map (kbd "M-S-<down>") #'idate-dec-year)
    (define-key map (kbd "f") #'idate-inc-current)
    (define-key map (kbd "b") #'idate-dec-current)
    (define-key map (kbd "<left>")
                #'idate-prev-field)
    (define-key map (kbd "TAB")
                #'idate-next-field)
    (define-key map (kbd "<backtab>")
                #'idate-prev-field)
    (define-key map (kbd "<right>")
                #'idate-next-field)
    (define-key map (kbd "RET") #'idate-done)
    (define-key map (kbd "SPC") #'idate-next-field)
    (define-key map (kbd "0") #'idate-edit-field-at-point)
    (define-key map (kbd "1") #'idate-edit-field-at-point)
    (define-key map (kbd "2") #'idate-edit-field-at-point)
    (define-key map (kbd "3") #'idate-edit-field-at-point)
    (define-key map (kbd "4") #'idate-edit-field-at-point)
    (define-key map (kbd "5") #'idate-edit-field-at-point)
    (define-key map (kbd "6") #'idate-edit-field-at-point)
    (define-key map (kbd "7") #'idate-edit-field-at-point)
    (define-key map (kbd "8") #'idate-edit-field-at-point)
    (define-key map (kbd "9") #'idate-edit-field-at-point)
    (define-key map (kbd "C->") #'idate-toggle-with-time)
    (define-key map (kbd "C-.") #'idate-today)
    (define-key map [remap (delete-backward-char)]
                #'left-char)
    map)
  "Keymap for interactive date entry in minibuffer.")

(defun idate-field-bounds-at-point ()
  "Find bounds of `field-name' text property at point."
  (idate-get-prop-bounds 'field-name))

(defun idate-get-prop-bounds (prop &optional pos)
  "Find text property PROP's start and end positions.

Argument PROP is the text property to search for.

Optional argument POS is the buffer position to start the search; defaults to
the current point position."
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
  "Move point to the next date field."
  (interactive)
  (when-let ((bounds (idate-field-bounds-at-point)))
    (goto-char (cdr bounds))
    (when-let ((found (next-single-char-property-change (point) 'field-name)))
      (goto-char found))))

(defun idate-prev-field ()
  "Move point to the previous date field."
  (interactive)
  (require 'text-property-search)
  (when-let ((bounds (idate-field-bounds-at-point)))
    (goto-char (car bounds))
    (text-property-search-backward 'field-name)))

(defun idate-field-name-at-point ()
  "Retrieve `field-name' text property at point."
  (when-let ((bounds (idate-get-prop-bounds 'field-name)))
    (get-text-property (car bounds) 'field-name)))

(defun idate-edit-field-at-point ()
  "Edit date field value at cursor position."
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
        (when-let ((val (idate-get-field-valid-value
                         field-name
                         (string-to-number new-value))))
          (idate-set-field-value
           field-name val)
          (idate-rerender)
          (if
              (when-let* ((pos (point))
                          (next-field-pos
                           (next-single-char-property-change pos
                                                             'field-name)))
                (= 1 (- next-field-pos pos)))
              (idate-next-field)
            (forward-char 1)))
        (when idate-popup-calendar
          (idate-calendar-eval))))))

(defvar idate-ovl (make-overlay 1 1)
  "Overlay for displaying the interactive date.")

(overlay-put idate-ovl 'face 'org-date-selected)

(delete-overlay idate-ovl)

(defun idate--eval-in-calendar (form)
  "Evaluate FORM in calendar buffer and highlight date.

Argument FORM is an Emacs Lisp expression to be evaluated in the context of the
calendar buffer."
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
  "Display a calendar for date selection."
  (save-excursion
    (let ((target
           (or (get-buffer-window calendar-buffer)
               (let ((ignore-window-parameters t))
                 (split-window
                  (frame-root-window) -10 'below)))))
      (with-selected-window target
        (pop-to-buffer-same-window (get-buffer-create calendar-buffer))
        (calendar-mode)
        (let* ((date (calendar-current-date))
               (month (calendar-extract-month date))
               (year (calendar-extract-year date)))
          (calendar-increment-month month year (- calendar-offset))
          (pop-to-buffer calendar-buffer)
          (calendar-generate-window month year)
          (idate--eval-in-calendar '(setq cursor-type nil))
          (unwind-protect
              (progn
                (calendar-forward-day (-
                                       (time-to-days
                                        (apply #'encode-time
                                               idate-current-time))
                                       (calendar-absolute-from-gregorian
                                        (calendar-current-date))))
                (idate--eval-in-calendar nil)
                (let ((map (copy-keymap calendar-mode-map)))
                  (define-key map (kbd "RET") #'idate-calendar-select)
                  (define-key map [mouse-1] #'idate-calendar-select-mouse)
                  (define-key map [mouse-2] #'idate-calendar-select-mouse)
                  (use-local-map map)))
            (bury-buffer calendar-buffer))))))
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(defun idate-calendar-select ()
  "Select date from calendar and update minibuffer."
  (interactive)
  (when-let ((date (calendar-cursor-to-date)))
    (pcase-let ((`(,month ,day ,year)
                 date))
      (idate-set-fields-value
       (list (cons 'month month)
             (cons 'day day)
             (cons 'year year))))
    (when (active-minibuffer-window)
      (with-selected-window (active-minibuffer-window)
        (idate-rerender)))))

(defun idate-calendar-select-mouse (ev)
  "Set date fields from calendar click and update minibuffer.

Argument EV is the event object representing the mouse event."
  (interactive "e")
  (mouse-set-point ev)
  (when-let ((date (calendar-cursor-to-date)))
    (pcase-let ((`(,month ,day ,year)
                 date))
      (idate-set-fields-value
       (list (cons 'month month)
             (cons 'day day)
             (cons 'year year))))
    (when (active-minibuffer-window)
      (with-selected-window (active-minibuffer-window)
        (idate-rerender)))))

(defun idate-rerender ()
  "Update minibuffer display with formatted date-time."
  (when (minibufferp)
    (let ((inhibit-read-only t)
          (pos (point)))
      (delete-region (minibuffer-prompt-end)
                     (point-max))
      (insert (idate-time-render-value
               (encode-time idate-current-time)))
      (goto-char pos)
      (add-text-properties (point-min)
                           (point-max) '(read-only t))
      (when idate-popup-calendar
        (let ((calendar-setup nil)
              (mouse-autoselect-window nil)
              (calendar-move-hook nil)
              (calendar-view-diary-initially-flag nil)
              (calendar-view-holidays-initially-flag nil)
              (calendar-mark-holidays-flag nil))
          (idate-calendar-eval))))))

(defun idate-time-render-value (time)
  "Render formatted date-time string from TIME with properties.

Argument TIME is a time value that can be a list, as returned by `decode-time',
or a TIME string."
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
                                            encoded))))
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
  "PROMPT for a date, then return it formatted.

Optional argument PROMPT is a string to display as the prompt in the minibuffer.

Optional argument DEFAULT-VALUE is the default time value to use if none is
provided.

Optional argument WITHOUT-TIME specifies whether to exclude time components from
the date.

Optional argument FORMAT-STR is a string specifying the format to use for
output."
  (format-time-string
   (or format-str
       (let ((default-format-cell
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

(defun idate--format-plural (count singular-str)
  "Format COUNT with SINGULAR-STR, adding \"s\" for plural.

Argument COUNT is an integer representing the quantity to consider for
pluralization.

Argument SINGULAR-STR is a string representing the singular form of the word to
be potentially pluralized."
  (concat (format "%d " count)
          (concat singular-str
                  (if (= count 1) "" "s"))))

(defun idate-format-time-diff (time)
  "Format a human-readable string representing TIME difference.

Argument TIME is a time value representing the number of seconds since the epoch
\\=(January 1, 1970, 00:00:00 GMT)."
  (let ((diff-secs
         (- (float-time (encode-time (append (list 0)
                                             (cdr (decode-time
                                                   (current-time))))))
            (float-time
             (encode-time (append (list 0)
                                  (cdr (decode-time time))))))))
    (if (zerop (round diff-secs))
        "Now"
      (let* ((past (> diff-secs 0))
             (diff-secs-int (if past diff-secs (- diff-secs)))
             (suffix (if past "ago" "from now"))
             (minutes-secs 60)
             (hours-secs (* 60 minutes-secs))
             (day-secs (* 24 hours-secs))
             (month-secs (* 30 day-secs))
             (year-secs (* 365 day-secs))
             (res
              (cond ((< diff-secs-int minutes-secs)
                     (idate--format-plural (truncate diff-secs-int) "second"))
                    ((< diff-secs-int hours-secs)
                     (idate--format-plural (truncate (/ diff-secs-int
                                                        minutes-secs))
                                           "minute"))
                    ((< diff-secs-int day-secs)
                     (idate--format-plural (truncate
                                            (/ diff-secs-int hours-secs))
                                           "hour"))
                    ((< diff-secs-int month-secs)
                     (idate--format-plural (truncate (/ diff-secs-int day-secs))
                                           "day"))
                    ((< diff-secs-int year-secs)
                     (idate--format-plural (truncate
                                            (/ diff-secs-int month-secs))
                                           "month"))
                    (t
                     (let* ((months (truncate (/ diff-secs-int month-secs)))
                            (years (/ months 12))
                            (remaining-months (% months 12)))
                       (string-join
                        (delq nil
                              (list
                               (when (> years 0)
                                 (idate--format-plural years "year"))
                               (when (> remaining-months 0)
                                 (idate--format-plural remaining-months "month"))))
                        " "))))))
        (concat res " " suffix)))))

(defun idate-show-diff-time ()
  "Display the time difference from `idate-current-time'."
  (message (idate-format-time-diff (encode-time idate-current-time))))

;;;###autoload
(defun idate-read (&optional prompt default-value without-time)
  "PROMPT for a date input with optional time components.

Optional argument PROMPT is the string to display as the prompt in the
minibuffer.

Optional argument DEFAULT-VALUE is the default time value to use if none is
provided.

Optional argument WITHOUT-TIME specifies whether to exclude time components
\(hour, minute, second) from the date."
  (setq idate-current-time
        (decode-time (or default-value (org-current-time))))
  (catch 'done
    (minibuffer-with-setup-hook
        (lambda ()
          (when (minibufferp)
            (add-hook 'post-command-hook #'idate-show-diff-time nil t)
            (use-local-map
             (make-composed-keymap idate-minubuffer-keymap
                                   minibuffer-local-map))
            (setq-local idate-rules
                        (if without-time (seq-remove
                                          (lambda (it)
                                            (memq
                                             (car
                                              it)
                                             '(hour
                                               minute
                                               second)))
                                          idate-rules-alist)
                          (seq-copy idate-rules-alist)))
            (let ((inhibit-read-only t))
              (idate-rerender))))
      (read-from-minibuffer (or prompt "Date: ")))))

(defun idate-insert-time-stamp (&optional without-hm inactive pre post extra)
  "Insert a timestamp at point with optional settings.

Optional argument WITHOUT-HM is a boolean value; if non-nil, the time is omitted
from the timestamp.

Optional argument INACTIVE is a boolean value; if non-nil, the timestamp is
inserted as inactive.

Optional argument PRE is a string to be inserted before the timestamp.

Optional argument POST is a string to be inserted after the timestamp.

Optional argument EXTRA is additional data to be passed to
`org-insert-time-stamp'."
  (let* ((with-time (symbol-value 'idate--org-with-time))
         (date (idate-read "Timestamp: " nil without-hm)))
    (when (not (equal with-time idate--org-with-time))
      (setq without-hm (not idate--org-with-time)))
    (org-insert-time-stamp date
                           (not without-hm) inactive pre post extra)))


(defvar org-read-date-final-answer)

(defun idate-org-read-date (&optional with-time to-time from-string prompt
                                      default-time default-input _inactive)
  "Parse and return a date-time string from user input.

This is a replacement for `org-read-date'.

\\(advice-add \\='org-read-date :override #\\='idate-org-read-date)

Optional argument WITH-TIME is a boolean indicating whether time should be
included in the date prompt.

Optional argument TO-TIME is a boolean indicating whether the function should
return the date as an encoded time value.

Optional argument FROM-STRING is a string representing a date that bypasses the
interactive prompt.

Optional argument PROMPT is a string used as the prompt for the date input.

Optional argument DEFAULT-TIME is the default time value used if no input is
provided.

Optional argument DEFAULT-INPUT is a string representing the default date input.

Optional argument _INACTIVE is ignored and not used in the function."
  (setq idate--org-with-time
        with-time)
  (let* ((org-with-time with-time)
         (org-time-stamp-rounding-minutes
          (if (equal org-with-time '(16))
              '(0 0)
            org-time-stamp-rounding-minutes))
         (ct (org-current-time))
         (org-def (or org-overriding-default-time default-time ct))
         (org-defdecode (decode-time org-def))
         (mouse-autoselect-window nil)  ; Don't let the mouse jump
         (calendar-setup nil)
         (calendar-move-hook nil)
         (calendar-view-diary-initially-flag nil)
         (calendar-view-holidays-initially-flag nil)
         (calendar-mark-holidays-flag nil)
         ans final)
    (when (< (nth 2 org-defdecode) org-extend-today-until)
      (setf (nth 2 org-defdecode) -1)
      (setf (nth 1 org-defdecode) 59)
      (setq org-def (org-encode-time org-defdecode))
      (setq org-defdecode (decode-time org-def)))
    (let* ((timestr (format-time-string
                     (if org-with-time "%Y-%m-%d %H:%M" "%Y-%m-%d")
                     org-def))
           (prompt (concat (if prompt (concat prompt " ") "")
                           (format "Date+time [%s]: " timestr))))
      (cond ((and from-string)
             (setq ans from-string))
            ((and default-input)
             (unwind-protect
                 (setq ans (read-string prompt default-input
                                        'org-read-date-history timestr))
               (when org-read-date-overlay
                 (delete-overlay org-read-date-overlay)
                 (setq org-read-date-overlay nil))))
            (t
             (let ((result (idate-read prompt org-def
                                       (not
                                        org-with-time))))
               (setq ans
                     (format-time-string (if idate--org-with-time
                                             "%Y-%m-%d %H:%M"
                                           "%Y-%m-%d")
                                         result)))))
      (setq final (org-read-date-analyze ans org-def org-defdecode))
      (when org-read-date-analyze-forced-year
        (message "Year was forced into %s"
                 (if org-read-date-force-compatible-dates
                     "compatible range (1970-2037)"
                   "range representable on this machine"))
        (ding))
      (setq final (org-encode-time final))
      (setq org-read-date-final-answer ans)
      (if to-time
          final
        (setq final (decode-time final))
        (if (and (boundp 'org-time-was-given) org-time-was-given)
            (format "%04d-%02d-%02d %02d:%02d"
                    (nth 5 final)
                    (nth 4 final)
                    (nth 3 final)
                    (nth 2 final)
                    (nth 1 final))
          (format "%04d-%02d-%02d" (nth 5 final)
                  (nth 4 final)
                  (nth 3 final)))))))

;;;###autoload
(defun idate-insert-org-time-stamp ()
  "Insert an Org-mode timestamp at point."
  (interactive)
  (idate-insert-time-stamp))

(provide 'idate)
;;; idate.el ends here
