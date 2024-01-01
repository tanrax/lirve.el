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
	(state 1) ;; 1: start, 2: check, 3: success
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
	(widget-button nil)
	(widget-message-success nil)
	(text-success "Nice!")
	(widget-button-quit nil)
	(text-button-quit "Quit")
	(widget-button-replay nil)
	(text-button-replay "New challenge")
	)
    ;; Imports
    (require 'widget)
    (eval-when-compile
      (require 'wid-edit))

    ;; Functions

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
      ;; Cursor to end
      (goto-char (point-max))
      ;; Text success
      (setq widget-message-success (widget-create 'item
						  text-success
						  ))
      ;; Replay button
      (setq widget-button-replay (widget-create 'push-button
						:size 20
						:widget-push-button-prefix "\n\n%["
						:widget-push-button-suffix  "%]\n\n"
						:notify (lambda (&rest ignore)
							  (start))
						text-button-replay))
      ;; Quit button
      (setq widget-button-quit (widget-create 'push-button
					      :size 20
					      :notify (lambda (&rest ignore))
					      text-button-quit)))


    (defun start ()
      "Start o replay challenge."
      (interactive)
      ;; Set the state
      (setq state 1)
      ;; Get a new verb
      (set-verb-to-learn)
      ;; Show the verb in infinitive
      (widget-value-set widget-item-verb (format-value-infinitive))
      ;; Update labels
      (update)
      ;; Focus on the first field
      (widget-forward 2))

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
      ;; Win
      (when (eq state 3)
	(toggle-layout-success)))

    (defun kill-app ()
      "Kill the application."
      (interactive)
      (kill-buffer buffer-name))

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
      (insert "\n\n")
      ;; Check button
      (setq widget-button (widget-create 'push-button
					 :notify (lambda (&rest ignore)
						   (update))
					 text-button-check))
      ;; Separator
      (insert "\n\n")
      (setq widget-message-success nil)
      ;; Replay button
      (setq widget-button-replay nil)
      ;; Quit button
      (setq widget-button-quit nil)

      ;; Display the buffer
      (use-local-map widget-keymap)
      (widget-setup))

    ;; Keybindings
    (define-key widget-keymap (kbd "q") 'kill-app)

    ;; Init
    (main-layout)
    (start)
    ))

(provide 'learning-irregular-verbs-in-English)

;;; learning-irregular-verbs-in-English.el ends here
(learning-irregular-verbs-in-English)
