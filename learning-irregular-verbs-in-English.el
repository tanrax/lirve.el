;;; learning-irregular-verbs-in-English.el --- Application to learn and review irregular verbs in English. -*- lexical-binding: t;
;;
;; Copyright © 2024 Andros Fenollosa
;; Authors: Andros Fenollosa <andros@fenollosa.email>
;; URL: https://github.com/tanrax/learning-irregular-verbs-in-English.el
;; Version: 1.0.0
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Application to learn and review irregular verbs in English.

;;; Code:

(defun learning-irregular-verbs-in-English ()
  "Application to learn and review irregular verbs in English."
  (interactive)
  (let ((verbs '(("be" "was/were" "been")
		 ("beat" "beat" "beaten")
		 ("become" "became" "become")
		 ("begin" "began" "begun")
		 ("bend" "bent" "bent")
		 ("bet" "bet" "bet")
		 ("bite" "bit" "bitten")
		 ("blow" "blew" "blown")
		 ("break" "broke" "broken")
		 ("bring" "brought" "brought")
		 ("build" "built" "built")
		 ("burn" "burnt" "burnt")
		 ("buy" "bought" "bought")
		 ("catch" "caught" "caught")
		 ("choose" "chose" "chosen")
		 ("come" "came" "come")
		 ("cost" "cost" "cost")
		 ("cut" "cut" "cut")
		 ("dig" "dug" "dug")
		 ("do" "did" "done")
		 ("draw" "drew" "drawn")
		 ("dream" "dreamt" "dreamt")
		 ("drink" "drank" "drunk")
		 ("drive" "drove" "driven")
		 ("eat" "ate" "eaten")
		 ("fall" "fell" "fallen")
		 ("feed" "fed" "fed")
		 ("feel" "felt" "felt")
		 ("fight" "fought" "fought")
		 ("find" "found" "found")
		 ("fly" "flew" "flown")
		 ("forget" "forgot" "forgotten")
		 ("forgive" "forgave" "forgiven")
		 ("freeze" "froze" "frozen")
		 ("get" "got" "got")
		 ("give" "gave" "given")
		 ("go" "went" "gone")
		 ("grow" "grew" "grown")
		 ("hang" "hung" "hung")
		 ("have" "had" "had")
		 ("hear" "heard" "heard")
		 ("hide" "hid" "hidden")
		 ("hit" "hit" "hit")

		 ))
	(buffer-name "*Learning irregular verbs in English*")
	(state 1) ;; 1: start, 2: playing (before first check), 3: win
	(verb-to-learn-infinitive nil)
	(verb-to-learn-simple-past nil)
	(verb-to-learn-past-participle nil)
	(emoji-valid "✅")
	(emoji-error "👎")
	(widget-title nil)
	(text-title " 🧑‍🎓 Learning irregular verbs in English 🇬🇧")
	(widget-item-verb nil)
	(widget-field-simple-past nil)
	(widget-label-check-simple-past nil)
	(widget-field-past-participle nil)
	(widget-label-check-past-participle nil)
	(text-button-check "Check")
	(widget-button-check nil)
	(widget-message-success nil)
	(widget-item-space-before-success nil)
	(text-success "Nice!")
	(widget-item-space-after-success nil)
	(widget-button-quit nil)
	(text-button-quit "Quit")
	(widget-item-space-between-buttons nil)
	(widget-button-replay nil)
	(text-button-replay "New challenge")
	)
    ;; Imports
    (require 'widget)
    (eval-when-compile
      (require 'wid-edit))

    ;; Functions

    (defun kill-app ()
      "Kill the application."
      (interactive)
      (kill-buffer buffer-name))

    (defun set-verb-to-learn ()
      "Set the verb to learn."
      (let ((verbs-random (nth (random (length verbs)) verbs)))
	(setq verb-to-learn-infinitive (nth 0 verbs-random))
	(setq verb-to-learn-simple-past (nth 1 verbs-random))
	(setq verb-to-learn-past-participle (nth 2 verbs-random))))

    (defun format-value-infinitive ()
      "Format the value of the infinitive."
      (format "Infinitive      ➡️ %s" verb-to-learn-infinitive))

    (defun format-check-simple-past ()
      "Format the value of the simple past."
      (if (eq state 1)
	  ""
	(format " %s" (if
				(and
				 (string= (widget-value widget-field-simple-past) verb-to-learn-simple-past)
				 (not (string= (widget-value widget-field-simple-past) "")))
				emoji-valid emoji-error))))

    (defun format-check-past-participle ()
      "Format the value of the past participle."
      (if (eq state 1)
	  ""
	(format " %s" (if
				(and
				 (string= (widget-value widget-field-past-participle) verb-to-learn-past-participle)
				 (not (string= (widget-value widget-field-past-participle) "")))
				emoji-valid emoji-error))))

    (defun toggle-layout-success ()
      "Toggle the layout to success."
      (if (eq state 3)
	  (progn
	    ;; Cursor to end
	    (goto-char (point-max))
	    ;; Remove check button
	    (widget-delete widget-button-check)
	    (setq widget-button-check nil)
	    ;; Text success
	    (setq widget-item-space-before-success (widget-create 'item
								  ""))
	    (setq widget-message-success (widget-create 'item
							text-success
							))
	    (setq widget-item-space-after-success (widget-create 'item
								 "\n"))
	    ;; Replay button
	    (setq widget-button-replay (widget-create 'push-button
						      :size 20
						      :notify (lambda (&rest ignore)
								(replay))
						      text-button-replay))
	    ;; Space
	    (setq widget-item-space-between-buttons (widget-create 'item
								   "\n"))
	    ;; Quit button
	    (setq widget-button-quit (widget-create 'push-button
						    :size 20
						    :notify (lambda (&rest ignore)
							      (kill-app))
						    text-button-quit))
	    (widget-backward 2)
	    )
	(progn
	  (when (not (eq widget-item-space-before-success nil)) (widget-delete widget-item-space-before-success))
	  (when (not (eq widget-message-success nil)) (widget-delete widget-message-success))
	  (when (not (eq widget-item-space-after-success nil)) (widget-delete widget-item-space-after-success))
	  (when (not (eq widget-button-replay nil)) (widget-delete widget-button-replay))
	  (when (not (eq widget-item-space-between-buttons nil)) (widget-delete widget-item-space-between-buttons))
	  (when (not (eq widget-button-quit nil)) (widget-delete widget-button-quit))
	  )))

    (defun make-button-check ()
      "Make the button check."
      (setq widget-button-check (widget-create 'push-button
					 :notify (lambda (&rest ignore)
						   (update))
					 text-button-check)))


    (defun start ()
      "Start o replay challenge."
      (interactive)
      ;; Set the state
      (setq state 1)
      ;; Get a new verb
      (set-verb-to-learn)
      ;; Show the verb in infinitive
      (widget-value-set widget-item-verb (format-value-infinitive))
      ;; Reset button check
      (when (eq widget-button-check nil) (make-button-check))
      ;; Clear the fields
      (widget-value-set widget-field-simple-past "")
      (widget-value-set widget-label-check-simple-past "")
      (widget-value-set widget-field-past-participle "")
      (widget-value-set widget-label-check-past-participle "")
      ;; Update labels
      (update))

    (defun replay ()
      "Replay the challenge."
      (interactive)
      (start)
      (widget-backward 1))

    (defun update ()
      "Check the answers."
      (interactive)
      ;; Is playing?
      (when (and (eq state 1)
		 (or
		  (not (string= (widget-value widget-field-simple-past) ""))
		  (not (string= (widget-value widget-field-past-participle) "")))
		 )
	(setq state 2))
      ;; Check the answers
      (when (eq state 2)
	;; Is win?
	(when (and
	       (string= (widget-value widget-field-simple-past) verb-to-learn-simple-past)
	       (string= (widget-value widget-field-past-participle) verb-to-learn-past-participle))
	  ;; Set the state
	  (setq state 3))
	;; Update the check labels
	(widget-value-set widget-label-check-simple-past (format-check-simple-past))
	(widget-value-set widget-label-check-past-participle (format-check-past-participle))
	)
      ;; Update the success layout if needed
      (toggle-layout-success))

    (defun main-layout ()
      "Make widgets for the main layout."
      (interactive)
      ;; Create the buffer
      (switch-to-buffer buffer-name)
      ;; Clear the buffer
      (kill-all-local-variables)
      (let ((inhibit-read-only t))
	(erase-buffer))
      (remove-overlays)
      ;; Create the widgets
      ;; Title
      (insert (propertize (format "\n%s\n\n" text-title) 'face '(:height 1.2 :weight bold)))
      ;; Verb in infinitive
      (setq widget-item-verb (widget-create 'item
					    :value ""))
      ;; Separator
      (insert "\nSimple past     ➡️ ")
      ;; Simple past
      (setq widget-field-simple-past (widget-create 'editable-field
						    :size 8
						    :help-echo "Type a Simple past"
						    ))
      ;; Label check
      (insert " ")
      (setq widget-label-check-simple-past (widget-create 'item
							  (format-check-simple-past)))
      ;; Separator
      (insert "\nPast participle ➡️ ")
      ;; Past participle
      (setq widget-field-past-participle (widget-create 'editable-field
							:size 8
							:help-echo "Type a Past participle"))
      ;; Label check
      (insert " ")
      (setq widget-label-check-past-participle (widget-create 'item
							      (format-check-past-participle)))
      ;; Separator
      (insert "\n")
      ;; Check button
      (make-button-check)
      ;; Display the buffer
      (use-local-map widget-keymap)
      (widget-setup))

    ;; Keybindings
    (define-key widget-keymap (kbd "q") 'kill-app)

    ;; Init
    (main-layout)
    (start)
    (widget-backward 1)
    ))

(provide 'learning-irregular-verbs-in-English)

;;; learning-irregular-verbs-in-English.el ends here
(learning-irregular-verbs-in-English)
