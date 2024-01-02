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
  (let* ((learning-irregular-verbs-in-English--verbs '(
						      ("beat" "beat" "beaten")
						      ("become" "became" "become")
						      ("begin" "began" "begun")
						      ("bend" "bent" "bent")
						      ("bet" "bet" "bet")
						      ("bite" "bit" "bitten")
						      ("bleed" "bled" "bled")
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
						      ("hold" "held" "held")
						      ("hurt" "hurt" "hurt")
						      ("keep" "kept" "kept")
						      ("know" "knew" "known")
						      ("lay" "laid" "laid")
						      ("lead" "led" "led")
						      ("learn" "learnt" "learnt")
						      ("leave" "left" "left")
						      ("lend" "lent" "lent")
						      ("let" "let" "let")
						      ("lie" "lay" "lain")
						      ("light" "lit" "lit")
						      ("lose" "lost" "lost")
						      ("make" "made" "made")
						      ("mean" "meant" "meant")
						      ("meet" "met" "met")
						      ("pay" "paid" "paid")
						      ("put" "put" "put")
						      ("read" "read" "read")
						      ("ride" "rode" "ridden")
						      ("ring" "rang" "rung")
						      ("rise" "rose" "risen")
						      ("run" "ran" "run")
						      ("say" "said" "said")
						      ("see" "saw" "seen")
						      ("sell" "sold" "sold")
						      ("send" "sent" "sent")
						      ("set" "set" "set")
						      ("shake" "shook" "shaken")
						      ("shine" "shone" "shone")
						      ("shoot" "shot" "shot")
						      ("show" "showed" "shown")
						      ("shut" "shut" "shut")
						      ("sing" "sang" "sung")
						      ("sink" "sank" "sunk")
						      ("sit" "sat" "sat")
						      ("sleep" "slept" "slept")
						      ("smell" "smelt" "smelt")
						      ("speak" "spoke" "spoken")
						      ("spend" "spent" "spent")
						      ("spill" "spelt" "spelt")
						      ("spit" "spit" "spit")
						      ("stand" "stood" "stood")
						      ("steal" "stole" "stolen")
						      ("swim" "swam" "swum")
						      ("take" "took" "taken")
						      ("teach" "taught" "taught")
						      ("tear" "tore" "torn")
						      ("tell" "told" "told")
						      ("think" "thought" "thought")
						      ("throw" "threw" "thrown")
						      ("understand" "understood" "understood")
						      ("wake" "woke" "woken")
						      ("wear" "wore" "worn")
						      ("win" "won" "won")
						      ("write" "wrote" "written")))
	;; learning-irregular-verbs-in-English
	(learning-irregular-verbs-in-English--buffer-name "*Learning irregular verbs in English*")
	(learning-irregular-verbs-in-English--state 1) ;; 1: start, 2: playing (before first check), 3: win (show success layout)
	(learning-irregular-verbs-in-English--verb-to-learn-infinitive nil)
	(learning-irregular-verbs-in-English--verb-to-learn-simple-past nil)
	(learning-irregular-verbs-in-English--verb-to-learn-past-participle nil)
	(learning-irregular-verbs-in-English--emoji-valid "✅")
	(learning-irregular-verbs-in-English--emoji-error "👎")
	(learning-irregular-verbs-in-English--widget-title nil)
	(learning-irregular-verbs-in-English--text-title " 🧑‍🎓 Learning irregular verbs in English 🇬🇧")
	(learning-irregular-verbs-in-English--widget-item-verb nil)
	(learning-irregular-verbs-in-English--widget-field-simple-past nil)
	(learning-irregular-verbs-in-English--widget-label-check-simple-past nil)
	(learning-irregular-verbs-in-English--widget-field-past-participle nil)
	(learning-irregular-verbs-in-English--widget-label-check-past-participle nil)
	(learning-irregular-verbs-in-English--text-button-check "Check")
	(learning-irregular-verbs-in-English--widget-button-check nil)
	(learning-irregular-verbs-in-English--widget-message-success nil)
	(learning-irregular-verbs-in-English--widget-item-space-before-success nil)
	(learning-irregular-verbs-in-English--text-success "Nice!")
	(learning-irregular-verbs-in-English--widget-item-space-after-success nil)
	(learning-irregular-verbs-in-English--widget-button-quit nil)
	(learning-irregular-verbs-in-English--text-button-quit "Quit")
	(learning-irregular-verbs-in-English--widget-item-space-between-buttons nil)
	(learning-irregular-verbs-in-English--widget-button-replay nil)
	(learning-irregular-verbs-in-English--text-button-replay "New challenge"))
    ;; Imports
    (require 'widget)
    (eval-when-compile
      (require 'wid-edit))

    ;; Functions

    (defun kill-app ()
      "Kill the application."
      (interactive)
      (kill-buffer learning-irregular-verbs-in-English--buffer-name))

    (defun value-field-simple-past ()
      (if (not (eq learning-irregular-verbs-in-English--widget-field-simple-past nil)) (widget-value learning-irregular-verbs-in-English--widget-field-simple-past) ""))

    (defun value-field-past-participle ()
      (if (not (eq learning-irregular-verbs-in-English--widget-field-past-participle nil)) (widget-value learning-irregular-verbs-in-English--widget-field-past-participle) ""))

    (defun set-verb-to-learn ()
      "Set the verb to learn."
      (let ((verbs-random (nth (random (length learning-irregular-verbs-in-English--verbs)) learning-irregular-verbs-in-English--verbs)))
	(setq learning-irregular-verbs-in-English--verb-to-learn-infinitive (nth 0 verbs-random))
	(setq learning-irregular-verbs-in-English--verb-to-learn-simple-past (nth 1 verbs-random))
	(setq learning-irregular-verbs-in-English--verb-to-learn-past-participle (nth 2 verbs-random))))

    (defun format-value-infinitive ()
      "Format the value of the infinitive."
      (format "Infinitive      ➡️ %s" learning-irregular-verbs-in-English--verb-to-learn-infinitive))

    (defun format-check-simple-past ()
      "Format the value of the simple past."
      (if (eq learning-irregular-verbs-in-English--state 1)
	  ""
	(format " %s" (if
				(and
				 (string= (value-field-simple-past) learning-irregular-verbs-in-English--verb-to-learn-simple-past)
				 (not (string= (value-field-simple-past) "")))
				learning-irregular-verbs-in-English--emoji-valid learning-irregular-verbs-in-English--emoji-error))))

    (defun format-check-past-participle ()
      "Format the value of the past participle."
      (if (eq learning-irregular-verbs-in-English--state 1)
	  ""
	(format " %s" (if
				(and
				 (string= (value-field-past-participle) learning-irregular-verbs-in-English--verb-to-learn-past-participle)
				 (not (string= (value-field-past-participle) "")))
				learning-irregular-verbs-in-English--emoji-valid learning-irregular-verbs-in-English--emoji-error))))

    (defun toggle-layout-success ()
      "Toggle the layout to success."
      (if (eq learning-irregular-verbs-in-English--state 3)
	  (progn
	    ;; Cursor to end
	    (goto-char (point-max))
	    ;; Remove check button
	    (widget-delete learning-irregular-verbs-in-English--widget-button-check)
	    (setq learning-irregular-verbs-in-English--widget-button-check nil)
	    ;; Text success
	    (setq learning-irregular-verbs-in-English--widget-item-space-before-success (widget-create 'item
								  ""))
	    (setq learning-irregular-verbs-in-English--widget-message-success (widget-create 'item
							learning-irregular-verbs-in-English--text-success
							))
	    (setq learning-irregular-verbs-in-English--widget-item-space-after-success (widget-create 'item
								 "\n"))
	    ;; Replay button
	    (setq learning-irregular-verbs-in-English--widget-button-replay (widget-create 'push-button
						      :size 20
						      :notify (lambda (&rest ignore)
								(replay))
						      learning-irregular-verbs-in-English--text-button-replay))
	    ;; Space
	    (setq learning-irregular-verbs-in-English--widget-item-space-between-buttons (widget-create 'item
								   "\n"))
	    ;; Quit button
	    (setq learning-irregular-verbs-in-English--widget-button-quit (widget-create 'push-button
						    :size 20
						    :notify (lambda (&rest ignore)
							      (kill-app))
						    learning-irregular-verbs-in-English--text-button-quit))
	    (widget-backward 2)
	    )
	(progn
	  (when (not (eq learning-irregular-verbs-in-English--widget-item-space-before-success nil)) (widget-delete learning-irregular-verbs-in-English--widget-item-space-before-success))
	  (when (not (eq learning-irregular-verbs-in-English--widget-message-success nil)) (widget-delete learning-irregular-verbs-in-English--widget-message-success))
	  (when (not (eq learning-irregular-verbs-in-English--widget-item-space-after-success nil)) (widget-delete learning-irregular-verbs-in-English--widget-item-space-after-success))
	  (when (not (eq learning-irregular-verbs-in-English--widget-button-replay nil)) (widget-delete learning-irregular-verbs-in-English--widget-button-replay))
	  (when (not (eq learning-irregular-verbs-in-English--widget-item-space-between-buttons nil)) (widget-delete learning-irregular-verbs-in-English--widget-item-space-between-buttons))
	  (when (not (eq learning-irregular-verbs-in-English--widget-button-quit nil)) (widget-delete learning-irregular-verbs-in-English--widget-button-quit))
	  )))

    (defun make-button-check ()
      "Make the button check."
      (setq learning-irregular-verbs-in-English--widget-button-check (widget-create 'push-button
					 :notify (lambda (&rest ignore)
						   (update))
					 learning-irregular-verbs-in-English--text-button-check)))


    (defun start ()
      "Start o replay challenge."
      (interactive)
      ;; Set the learning-irregular-verbs-in-English--state
      (setq learning-irregular-verbs-in-English--state 1)
      ;; Get a new verb
      (set-verb-to-learn)
      ;; Show the verb in infinitive
      (widget-value-set learning-irregular-verbs-in-English--widget-item-verb (format-value-infinitive))
      ;; Reset button check
      (when (eq learning-irregular-verbs-in-English--widget-button-check nil) (make-button-check))
      ;; Clear the fields
      (widget-value-set learning-irregular-verbs-in-English--widget-field-simple-past "")
      (widget-value-set learning-irregular-verbs-in-English--widget-label-check-simple-past "")
      (widget-value-set learning-irregular-verbs-in-English--widget-field-past-participle "")
      (widget-value-set learning-irregular-verbs-in-English--widget-label-check-past-participle "")
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
      (when (and (eq learning-irregular-verbs-in-English--state 1)
		 (or
		  (not (string= (value-field-simple-past) ""))
		  (not (string= (value-field-past-participle) "")))
		 )
	(setq learning-irregular-verbs-in-English--state 2))
      ;; Check the answers
      (when (eq learning-irregular-verbs-in-English--state 2)
	;; Is win?
	(when (and
	       (string= (value-field-simple-past) learning-irregular-verbs-in-English--verb-to-learn-simple-past)
	       (string= (value-field-past-participle) learning-irregular-verbs-in-English--verb-to-learn-past-participle))
	  ;; Set the learning-irregular-verbs-in-English--state
	  (setq learning-irregular-verbs-in-English--state 3))
	;; Update the check labels
	(widget-value-set learning-irregular-verbs-in-English--widget-label-check-simple-past (format-check-simple-past))
	(widget-value-set learning-irregular-verbs-in-English--widget-label-check-past-participle (format-check-past-participle)))
      ;; Update the success layout if needed
      (toggle-layout-success))

    (defun main-layout ()
      "Make widgets for the main layout."
      (interactive)
      ;; Create the buffer
      (switch-to-buffer learning-irregular-verbs-in-English--buffer-name)
      ;; Clear the buffer
      (kill-all-local-variables)
      (let ((inhibit-read-only t))
	(erase-buffer))
      (remove-overlays)
      ;; Create the widgets
      ;; Title
      (insert (propertize (format "\n%s\n\n" learning-irregular-verbs-in-English--text-title) 'face '(:height 1.2 :weight bold)))
      ;; Verb in infinitive
      (setq learning-irregular-verbs-in-English--widget-item-verb (widget-create 'item
					    :value ""))
      ;; Separator
      (insert "\nSimple past     ➡️ ")
      ;; Simple past
      (setq learning-irregular-verbs-in-English--widget-field-simple-past (widget-create 'editable-field
						    :size 8
						    :help-echo "Type a Simple past"
						    ))
      ;; Label check
      (insert " ")
      (setq learning-irregular-verbs-in-English--widget-label-check-simple-past (widget-create 'item
							  (format-check-simple-past)))
      ;; Separator
      (insert "\nPast participle ➡️ ")
      ;; Past participle
      (setq learning-irregular-verbs-in-English--widget-field-past-participle (widget-create 'editable-field
							:size 8
							:help-echo "Type a Past participle"))
      ;; Label check
      (insert " ")
      (setq learning-irregular-verbs-in-English--widget-label-check-past-participle (widget-create 'item
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
    (widget-backward 1)))

(provide 'learning-irregular-verbs-in-English)

;;; learning-irregular-verbs-in-English.el ends here
