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
		 ("hit" "hit" "hit")))
	(buffer-name "*Learning irregular verbs in English*")
	(verb-to-learn-infinitive nil)
	(verb-to-learn-simple-past nil)
	(verb-to-learn-past-participle nil)
	(emoji-valid "✅")
	(emoji-error "👎")
	(widget-title nil)
	(text-title " 🧑‍🎓 Learning irregular verbs in English 🇬🇧")
	(widget-item-verb nil)
	(widget-field-simple-past nil)
	(widget-field-past-participle nil)
	(text-button-check "Check")
	(widget-button nil)
	(widget-message-success nil)
	(text-success "Congratulations!")
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

    (defun format-value-infinitive (value)
      "Format the value of the infinitive."
      (format "Infinitive      ➡️ %s" value))

    (defun format-value-simple-past (value)
      "Format the value of the simple past."
      (format "Simple past ➡️ %s %s" value (if
					      (and
					       (string= value verb-to-learn-simple-past)
					       (not (string= value "")))
					      emoji-valid emoji-error)))

    (defun start-or-replay ()
      "Start o replay challenge."
      (interactive)
      ;; Get a new verb
      (set-verb-to-learn)
      ;; Show the verb in infinitive
      (widget-value-set widget-item-verb (format-value-infinitive verb-to-learn-infinitive))

      ;; Clear the fields
      (widget-value-set widget-field-simple-past "")
      (widget-value-set widget-field-past-participle "")
      ;; Focus on the first field
      (widget-forward 2))

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
      ;; Separator
      (insert "\n\nPast participle ➡️ ")
      ;; Past participle
      (setq widget-field-past-participle (widget-create 'editable-field
							:size 8
							:help-echo "Type a Past participle"))
      ;; Separator
      (insert "\n\n")
      ;; Check button
      (setq widget-button (widget-create 'push-button
					 text-button-check))
      ;; Separator
      (insert "\n\n")
      ;; Success message
      (setq widget-message-success (widget-create 'item
						  text-success
						  ))
      ;; Separator
      (insert "\n  ")
      ;; Replay button
      (setq widget-button-replay (widget-create 'push-button
						:size 20
						:notify (lambda (&rest ignore)
							  (start-or-replay))
						text-button-replay))
      ;; Separator
      (insert "  ")
      ;; Quit button
      (setq widget-button-quit (widget-create 'push-button
					      :size 20
					      :notify (lambda (&rest ignore))
					      text-button-quit))

      ;; Display the buffer
      (use-local-map widget-keymap)
      (widget-setup))

    ;; Keybindings
    (define-key widget-keymap (kbd "q") 'kill-app)

    ;; Init
    (main-layout)
    (start-or-replay)
    ))

(provide 'learning-irregular-verbs-in-English)

;;; learning-irregular-verbs-in-English.el ends here
(learning-irregular-verbs-in-English)
