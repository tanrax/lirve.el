;;; lirve.el --- Learn irregular verbs in English. -*- lexical-binding: t -*-
;;
;; Copyright © 2024 Andros Fenollosa
;; Authors: Andros Fenollosa <andros@fenollosa.email>
;; URL: https://github.com/tanrax/lirve.el
;; Version: 1.2.0
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:
;; Application to learn and review irregular verbs in English.
;; You can optionally configure Lirve to display translations.
;; Only available in Spanish (at the moment).
;; If you want to add more languages, make a PR with the translations in `lirve-verbs.el`.

;;; Code:

;; Imports
(require 'lirve-verbs)
(require 'widget)
(require 'wid-edit)
(eval-when-compile
  (require 'wid-edit))

;; Variables
(defvar lirve--count-verbs 0) ;; It's used to know when unresolved verbs are shown
(defvar lirve--interval-unresolved 3) ;; Interval to show unresolved verbs
(defvar lirve--verbs-shuffle '())
(defconst lirve--file-name-unresolved ".lirve-unresolved")
(defvar lirve--verbs-unresolved '())
(defconst lirve--buffer-name "*Learning irregular verbs in English*")
(defvar lirve--state :start) ;; :start -> Init, :playing before first check, :win show success layout
(defvar lirve--verb-to-learn-infinitive nil)
(defvar lirve--verb-to-learn-simple-past nil)
(defvar lirve--verb-to-learn-past-participle nil)
(defvar lirve--translation "")
(defvar lirve--set-translation nil) ;; Set the translation language
(defconst lirve--emoji-valid "✅")
(defconst lirve--emoji-error "👎")
(defvar lirve--widget-title nil)
(defvar lirve--text-title " 🧑‍🎓 Learning irregular verbs in English 🇬🇧")
(defvar lirve--widget-item-verb nil)
(defvar lirve--widget-field-simple-past nil)
(defvar lirve--widget-label-check-simple-past nil)
(defvar lirve--widget-field-past-participle nil)
(defvar lirve--widget-label-check-past-participle nil)
(defconst lirve--text-button-check "Check")
(defvar lirve--widget-button-check nil)
(defvar lirve--widget-item-space-before-check nil)
(defconst lirve--text-button-show-solution "Don't know")
(defvar lirve--widget-button-show-solution nil)
(defvar lirve--widget-message-success nil)
(defvar lirve--widget-item-space-before-success nil)
(defconst lirve--text-success "Nice!")
(defconst lirve--text-fail "Next time you will do better")
(defvar lirve--resolved-p t)
(defvar lirve--widget-item-space-after-success nil)
(defvar lirve--widget-button-quit nil)
(defconst lirve--text-button-quit "Quit")
(defvar lirve--widget-item-space-between-buttons nil)
(defvar lirve--widget-button-lirve--replay nil)
(defconst lirve--text-button-lirve--replay "New challenge")

;; Functions

(defun lirve-kill-app ()
  "Kill the application."
  (interactive)
  (kill-buffer lirve--buffer-name))

(defun lirve--it-have-decimals (num)
  "Return t if NUM is have decimals."
  (let ((my-num (if (and
		     (stringp num)) ;; Return 0 if it is not a number
		    (string-to-number num) num)))
    (when my-num (not (or (zerop my-num)    ;; Check if it is 0
			  (integerp my-num) ;; Check if it is integer
			  (and (floatp my-num) (equal my-num (float (truncate my-num)))) ;; Check if it is float
			  )))))

(defun lirve--shuffle (original-list &optional shuffled-list)
  "Apply the Fisher-Yates shuffle algorithm.
The parameter SHUFFLED-LIST is used for recursion
and should not be used by the user.
Example: (lirve--shuffle (list (1 2 3 4 5))) -> (3 1 5 2 4)
Argument ORIGINAL-LIST List to shuffle."
  (if (null original-list)
      ;; End recursion, return the shuffled list
      shuffled-list
    ;; Otherwise, continue with the logic
    (let* ((random-position (random (length original-list)))
           (random-element (nth random-position original-list))
           ;; Create a new original list without the randomly selected element
           (original-list-without-random-element (append (cl-subseq original-list 0 random-position) (nthcdr (1+ random-position) original-list)))
           ;; Create a new shuffled list with the selected element at the beginning
           (new-shuffled-list (if (null shuffled-list) (list random-element) (cons random-element shuffled-list))))
      ;; Recursively call the shuffle function with the new original list and the new shuffled list
      (lirve--shuffle original-list-without-random-element new-shuffled-list))))

(defun lirve--get-verb-for-infinitive (infinitive)
  "Get the verb for the INFINITIVE."
  (car (seq-filter
      (lambda (verb) (string= infinitive (cdr (assq 'infinitive verb))))
      lirve-verbs--list)))

(defun lirve--is-win ()
  "Return t if the state is win."
  (and
   (string= (lirve--value-field-simple-past) lirve--verb-to-learn-simple-past)
   (string= (lirve--value-field-past-participle) lirve--verb-to-learn-past-participle)))

(defun lirve--full-path-unresolved ()
  "Get the full path of the unresolved file."
  (concat (file-name-directory user-init-file) lirve--file-name-unresolved))

(defun lirve--save-verb-unresolved (infinitive)
  "Save the verb unresolved to `lirve--verbs-unresolved' and to the file.
Argument INFINITIVE is verb to learn."
  (when infinitive
    (setq lirve--verbs-unresolved (delete-dups (append lirve--verbs-unresolved (list infinitive))))
    (with-temp-file (lirve--full-path-unresolved)
      (prin1 lirve--verbs-unresolved (current-buffer)))))

(defun lirve--remove-verb-unresolved (infinitive)
  "Remove the verb unresolved from `lirve--verbs-unresolved' and from the file.
Argument INFINITIVE verb to remove."
  (setq lirve--verbs-unresolved (delete infinitive lirve--verbs-unresolved))
  (with-temp-file (lirve--full-path-unresolved)
    (prin1 lirve--verbs-unresolved (current-buffer))))

(defun lirve--load-verbs-unresolved ()
  "Load the unresolved verbs from the file."
  (when (file-exists-p (lirve--full-path-unresolved))
    (with-temp-buffer
      (insert-file-contents (lirve--full-path-unresolved))
      (setq lirve--verbs-unresolved (read (current-buffer))))))

(defun lirve--value-field-simple-past ()
  "Get the value of the simple past."
  (if lirve--widget-field-simple-past (widget-value lirve--widget-field-simple-past) ""))

(defun lirve--value-field-past-participle ()
  "Get the value of the past participle."
  (if lirve--widget-field-past-participle (widget-value lirve--widget-field-past-participle) ""))

(defun lirve--set-verb-to-learn ()
  "Set the verb to learn."
  ;; If the list is empty, shuffle it
  (when (null lirve--verbs-shuffle)
    (setq lirve--verbs-shuffle (lirve--shuffle lirve-verbs--list)))
  ;; Get verb
  (let* ((turn-unresolved (not (lirve--it-have-decimals (/ (float lirve--count-verbs) lirve--interval-unresolved)))) ;; Calculate if it is time to show unresolved verbs: Count / Interval. If it isn't a decimal, it is time to show unresolved verbs
	 (verb-to-learn
	  (if (and lirve--verbs-unresolved turn-unresolved)
	      (lirve--get-verb-for-infinitive (car lirve--verbs-unresolved))
	    (car lirve--verbs-shuffle))))
    (setq lirve--verb-to-learn-infinitive (cdr (assq 'infinitive verb-to-learn)))
    (setq lirve--verb-to-learn-simple-past (cdr (assq 'simple-past verb-to-learn)))
    (setq lirve--verb-to-learn-past-participle (cdr (assq 'past-participle verb-to-learn)))
    (when lirve--set-translation (setq lirve--translation (cdr (assq lirve--set-translation (cdr (assq 'translations verb-to-learn))))))
    ;; Remove the verb from the list
    (unless turn-unresolved
	(setq lirve--verbs-shuffle (cdr lirve--verbs-shuffle))))
  ;; Increase the count of verbs
  (setq lirve--count-verbs (1+ lirve--count-verbs)))

(defun lirve--format-value-infinitive ()
  "Format the value of the infinitive."
  (format "Infinitive      ➡️ %s" lirve--verb-to-learn-infinitive))

(defun lirve--format-check-simple-past ()
  "Format the value of the simple past."
  (if (eq lirve--state :start)
      ""
    (format " %s" (if
		      (and
		       (string= (lirve--value-field-simple-past) lirve--verb-to-learn-simple-past)
		       (not (string= (lirve--value-field-simple-past) "")))
		      lirve--emoji-valid lirve--emoji-error))))

(defun lirve--format-check-past-participle ()
  "Format the value of the past participle."
  (if (eq lirve--state :start)
      ""
    (format " %s" (if
		      (and
		       (string= (lirve--value-field-past-participle) lirve--verb-to-learn-past-participle)
		       (not (string= (lirve--value-field-past-participle) "")))
		      lirve--emoji-valid lirve--emoji-error))))

(defun lirve--show-translation ()
  "Show translation if `lirve--show-translation' is t."
  (when lirve--set-translation
    (widget-value-set lirve--widget-item-verb (concat (lirve--format-value-infinitive) "   🇪🇸 " lirve--translation))))

(defun lirve--toggle-layout-finish ()
  "Toggle the layout to success."
  (if (eq lirve--state :win)
      (progn
	;; Show translate
	(lirve--show-translation)
	;; Cursor to end
	(goto-char (point-max))
	;; Remove check button
	(widget-delete lirve--widget-button-check)
	(setq lirve--widget-button-check nil)
	;; Remove space after check button
	(widget-delete lirve--widget-item-space-before-check)
	(setq lirve--widget-item-space-before-check nil)
	;; Remove show solution button
	(widget-delete lirve--widget-button-show-solution)
	(setq lirve--widget-button-show-solution nil)
	;; Text success
	(setq lirve--widget-item-space-before-success (widget-create 'item
								    ""))
	(setq lirve--widget-message-success (widget-create 'item
							  (if lirve--resolved-p lirve--text-success lirve--text-fail)))
	(setq lirve--widget-item-space-after-success (widget-create 'item
								   "\n"))
	;; Lirve--Replay button
	(setq lirve--widget-button-lirve--replay (widget-create 'push-button
							      :size 20
							      :notify (lambda (&rest ignore)
									(lirve--replay))
							      lirve--text-button-lirve--replay))
	;; Space
	(setq lirve--widget-item-space-between-buttons (widget-create 'item
								     "\n"))
	;; Quit button
	(setq lirve--widget-button-quit (widget-create 'push-button
						      :size 20
						      :notify (lambda (&rest ignore) (lirve-kill-app))
						      lirve--text-button-quit))
	(widget-backward 2))
    (when lirve--widget-item-space-before-success (widget-delete lirve--widget-item-space-before-success))
    (when lirve--widget-message-success (widget-delete lirve--widget-message-success))
    (when lirve--widget-item-space-after-success (widget-delete lirve--widget-item-space-after-success))
    (when lirve--widget-button-lirve--replay (widget-delete lirve--widget-button-lirve--replay))
    (when lirve--widget-item-space-between-buttons (widget-delete lirve--widget-item-space-between-buttons))
    (when lirve--widget-button-quit (widget-delete lirve--widget-button-quit))))

(defun lirve--make-button-check ()
  "Make the button check."
  (setq lirve--widget-button-check (widget-create 'push-button
						 :notify (lambda (&rest ignore)
							   (setq lirve--resolved-p (lirve--is-win))
							   (lirve--update))
						 lirve--text-button-check)))
(defun lirve--make-space-after-check ()
  "Add space between Button check and Button show solution."
  (setq lirve--widget-item-space-before-check (widget-create 'item "\n")))


(defun lirve--show-solutions ()
  "Show solutions."
  ;; Show the solutions
  (widget-value-set lirve--widget-field-simple-past lirve--verb-to-learn-simple-past)
  (widget-value-set lirve--widget-field-past-participle lirve--verb-to-learn-past-participle)
  ;; Set state to lose
  (setq lirve--resolved-p nil))

(defun lirve--make-button-show-solution ()
  "Make the button show solution."
  (setq lirve--widget-button-show-solution (widget-create 'push-button
							 :notify (lambda (&rest ignore)
								   (setq lirve--resolved-p (lirve--is-win))
								   (lirve--show-solutions)
								   (lirve--update))
							 lirve--text-button-show-solution)))


(defun lirve--start ()
  "Start challenge."
  ;; Set the lirve--state
  (setq lirve--state :start)
  ;; Get a new verb
  (lirve--set-verb-to-learn)
  ;; Show the verb in infinitive
  (widget-value-set lirve--widget-item-verb (lirve--format-value-infinitive))
  ;; Reset button check
  (unless lirve--widget-button-check (lirve--make-button-check))
  ;; Reset space after check
  (unless lirve--widget-item-space-before-check (lirve--make-space-after-check))
  ;; Reset button show solution
  (unless lirve--widget-button-show-solution (lirve--make-button-show-solution))
  ;; Clear the fields
  (widget-value-set lirve--widget-field-simple-past "")
  (widget-value-set lirve--widget-label-check-simple-past "")
  (widget-value-set lirve--widget-field-past-participle "")
  (widget-value-set lirve--widget-label-check-past-participle "")
  ;; Update labels
  (lirve--update))

(defun lirve--replay ()
  "Replay the challenge."
  (interactive)
  (lirve--start)
  (widget-backward 1))

(defun lirve--update ()
  "Update state and show temps layouts."
  (interactive)
  ;; Is playing?
  (when (and (eq lirve--state :start)
	     (or
	      (not (string= (lirve--value-field-simple-past) ""))
	      (not (string= (lirve--value-field-past-participle) ""))))
    (setq lirve--state :playing))
  ;; Check the answers
  (when (eq lirve--state :playing)
    ;; Is win?
    (when (and
	   (string= (lirve--value-field-simple-past) lirve--verb-to-learn-simple-past)
	   (string= (lirve--value-field-past-participle) lirve--verb-to-learn-past-participle))
      ;; Add or remove from unresolved list
      (if lirve--resolved-p
	  (lirve--remove-verb-unresolved lirve--verb-to-learn-infinitive)
	(lirve--save-verb-unresolved lirve--verb-to-learn-infinitive))
      ;; Set the lirve--state
      (setq lirve--state :win))
    ;; Update the check labels
    (widget-value-set lirve--widget-label-check-simple-past (lirve--format-check-simple-past))
    (widget-value-set lirve--widget-label-check-past-participle (lirve--format-check-past-participle)))
  ;; Update the success layout if needed
  (lirve--toggle-layout-finish))

(defun lirve--main-layout ()
  "Make widgets for the main layout."
  ;; Create the buffer
  (switch-to-buffer lirve--buffer-name)
  ;; Clear the buffer
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  ;; Create the widgets
  ;; Title
  (insert (propertize (format "\n%s\n\n" lirve--text-title) 'face '(:height 1.2 :weight bold)))
  ;; Verb in infinitive
  (setq lirve--widget-item-verb (widget-create 'item
					       :value ""))
  ;; Separator
  (insert "\nSimple past     ➡️ ")
  ;; Simple past
  (setq lirve--widget-field-simple-past (widget-create 'editable-field
						       :size 8
						       :help-echo "Type a Simple past"))
  ;; Label check
  (insert " ")
  (setq lirve--widget-label-check-simple-past (widget-create 'item
							     (lirve--format-check-simple-past)))
  ;; Separator
  (insert "\nPast participle ➡️ ")
  ;; Past participle
  (setq lirve--widget-field-past-participle (widget-create 'editable-field
											 :size 8
											 :help-echo "Type a Past participle"))
  ;; Label check
  (insert " ")
  (setq lirve--widget-label-check-past-participle (widget-create 'item
											       (lirve--format-check-past-participle)))
  ;; Separator
  (insert "\n")
  ;; Check button
  (lirve--make-button-check)
    ;; Separator
  (lirve--make-space-after-check)
  ;; Show solution button
  (lirve--make-button-show-solution)
  ;; Display the buffer
  (use-local-map widget-keymap)
  ;; Disable line numbers
  (display-line-numbers-mode -1)
  (widget-setup))

;; Init
(defun lirve ()
  "Application to learn and review irregular verbs in English."
  (interactive)
  (lirve--load-verbs-unresolved)
  (lirve--main-layout)
  (lirve--start)
  (widget-backward 4))

(provide 'lirve)

;;; lirve.el ends here
