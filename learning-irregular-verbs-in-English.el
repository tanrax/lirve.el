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

;; Imports
(require 'widget)
(eval-when-compile
  (require 'wid-edit))

;; Variables

(defvar lire--verbs '(
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
		      ("spell" "spelt" "spelt")
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
;; lire
(defvar lire--buffer-name "*Learning irregular verbs in English*")
(defvar lire--state 1) ;; 1: lire--start, 2: playing (before first check), 3: win (show success layout)
(defvar lire--verb-to-learn-infinitive nil)
(defvar lire--verb-to-learn-simple-past nil)
(defvar lire--verb-to-learn-past-participle nil)
(defvar lire--emoji-valid "✅")
(defvar lire--emoji-error "👎")
(defvar lire--widget-title nil)
(defvar lire--text-title " 🧑‍🎓 Learning irregular verbs in English 🇬🇧")
(defvar lire--widget-item-verb nil)
(defvar lire--widget-field-simple-past nil)
(defvar lire--widget-label-check-simple-past nil)
(defvar lire--widget-field-past-participle nil)
(defvar lire--widget-label-check-past-participle nil)
(defvar lire--text-button-check "Check")
(defvar lire--widget-button-check nil)
(defvar lire--widget-item-space-before-check nil)
(defvar lire--text-button-show-solution "Don't know")
(defvar lire--widget-button-show-solution nil)
(defvar lire--widget-message-success nil)
(defvar lire--widget-item-space-before-success nil)
(defvar lire--text-success "Nice!")
(defvar lire--text-fail "Next time you will do better")
(defvar lire--is-resolve t)
(defvar lire--widget-item-space-after-success nil)
(defvar lire--widget-button-quit nil)
(defvar lire--text-button-quit "Quit")
(defvar lire--widget-item-space-between-buttons nil)
(defvar lire--widget-button-lire--replay nil)
(defvar lire--text-button-lire--replay "New challenge")

;; Functions

(defun lire--kill-app ()
  "Kill the application."
  (kill-buffer lire--buffer-name))

(defun lire--value-field-simple-past ()
  (if (not (eq lire--widget-field-simple-past nil)) (widget-value lire--widget-field-simple-past) ""))

(defun lire--value-field-past-participle ()
  (if (not (eq lire--widget-field-past-participle nil)) (widget-value lire--widget-field-past-participle) ""))

(defun lire--set-verb-to-learn ()
  "Set the verb to learn."
  (let ((verbs-random (nth (random (length lire--verbs)) lire--verbs)))
    (setq lire--verb-to-learn-infinitive (nth 0 verbs-random))
    (setq lire--verb-to-learn-simple-past (nth 1 verbs-random))
    (setq lire--verb-to-learn-past-participle (nth 2 verbs-random))))

(defun lire--format-value-infinitive ()
  "Format the value of the infinitive."
  (format "Infinitive      ➡️ %s" lire--verb-to-learn-infinitive))

(defun lire--format-check-simple-past ()
  "Format the value of the simple past."
  (if (eq lire--state 1)
      ""
    (format " %s" (if
		      (and
		       (string= (lire--value-field-simple-past) lire--verb-to-learn-simple-past)
		       (not (string= (lire--value-field-simple-past) "")))
		      lire--emoji-valid lire--emoji-error))))

(defun lire--format-check-past-participle ()
  "Format the value of the past participle."
  (if (eq lire--state 1)
      ""
    (format " %s" (if
		      (and
		       (string= (lire--value-field-past-participle) lire--verb-to-learn-past-participle)
		       (not (string= (lire--value-field-past-participle) "")))
		      lire--emoji-valid lire--emoji-error))))

(defun lire--toggle-layout-finish ()
  "Toggle the layout to success."
  (if (eq lire--state 3)
      (progn
	;; Cursor to end
	(goto-char (point-max))
	;; Remove check button
	(widget-delete lire--widget-button-check)
	(setq lire--widget-button-check nil)
	;; Remove space after check button
	(widget-delete lire--widget-item-space-before-check)
	(setq lire--widget-item-space-before-check nil)
	;; Remove show solution button
	(widget-delete lire--widget-button-show-solution)
	(setq lire--widget-button-show-solution nil)
	;; Text success
	(setq lire--widget-item-space-before-success (widget-create 'item
												   ""))
	(setq lire--widget-message-success (widget-create 'item
											 (if lire--is-resolve lire--text-success lire--text-fail)))
	(setq lire--widget-item-space-after-success (widget-create 'item
								   "\n"))
	;; Lire--Replay button
	(setq lire--widget-button-lire--replay (widget-create 'push-button
										       :size 20
										       :notify (lambda (&rest ignore)
												 (lire--replay))
										       lire--text-button-lire--replay))
	;; Space
	(setq lire--widget-item-space-between-buttons (widget-create 'item
												    "\n"))
	;; Quit button
	(setq lire--widget-button-quit (widget-create 'push-button
										     :size 20
										     :notify (lambda (&rest ignore)
											       (lire--kill-app))
										     lire--text-button-quit))
	(widget-backward 2)
	)
    (progn
      (when (not (eq lire--widget-item-space-before-success nil)) (widget-delete lire--widget-item-space-before-success))
      (when (not (eq lire--widget-message-success nil)) (widget-delete lire--widget-message-success))
      (when (not (eq lire--widget-item-space-after-success nil)) (widget-delete lire--widget-item-space-after-success))
      (when (not (eq lire--widget-button-lire--replay nil)) (widget-delete lire--widget-button-lire--replay))
      (when (not (eq lire--widget-item-space-between-buttons nil)) (widget-delete lire--widget-item-space-between-buttons))
      (when (not (eq lire--widget-button-quit nil)) (widget-delete lire--widget-button-quit))
      )))

(defun lire--make-button-check ()
  "Make the button check."
  (setq lire--widget-button-check (widget-create 'push-button
										:notify (lambda (&rest ignore)
											  (lire--update))
										lire--text-button-check)))
(defun lire--make-space-after-check ()
  "Add space between Button check and Button show solution"
  (setq lire--widget-item-space-before-check (widget-create 'item "\n")))


(defun lire--show-solutions ()
  "Show solutions"
  (setq lire--is-resolve nil)
  (widget-value-set lire--widget-field-simple-past lire--verb-to-learn-simple-past)
  (widget-value-set lire--widget-field-past-participle lire--verb-to-learn-past-participle))

(defun lire--make-button-show-solution ()
  "Make the button show solution."
  (setq lire--widget-button-show-solution (widget-create 'push-button
							 :notify (lambda (&rest ignore)
								   (lire--show-solutions)
								   (lire--update))
							 lire--text-button-show-solution)))


(defun lire--start ()
  "Start challenge."
  ;; Set the lire--state
  (setq lire--state 1)
  ;; Get a new verb
  (lire--set-verb-to-learn)
  ;; Show the verb in infinitive
  (widget-value-set lire--widget-item-verb (lire--format-value-infinitive))
  ;; Reset button check
  (when (eq lire--widget-button-check nil) (lire--make-button-check))
  ;; Reset space after check
  (when (eq lire--widget-item-space-before-check nil) (lire--make-space-after-check))
  ;; Reset button show solution
  (when (eq lire--widget-button-show-solution nil) (lire--make-button-show-solution))
  ;; Clear the fields
  (widget-value-set lire--widget-field-simple-past "")
  (widget-value-set lire--widget-label-check-simple-past "")
  (widget-value-set lire--widget-field-past-participle "")
  (widget-value-set lire--widget-label-check-past-participle "")
  ;; Update labels
  (lire--update))

(defun lire--replay ()
  "Replay the challenge."
  (interactive)
  (lire--start)
  (widget-backward 1))

(defun lire--update ()
  "Update state and show temps layouts."
  (interactive)
  ;; Is playing?
  (when (and (eq lire--state 1)
	     (or
	      (not (string= (lire--value-field-simple-past) ""))
	      (not (string= (lire--value-field-past-participle) "")))
	     )
    (setq lire--state 2))
  ;; Check the answers
  (when (eq lire--state 2)
    ;; Is win?
    (when (and
	   (string= (lire--value-field-simple-past) lire--verb-to-learn-simple-past)
	   (string= (lire--value-field-past-participle) lire--verb-to-learn-past-participle))
      ;; Set the lire--state
      (setq lire--state 3))
    ;; Update the check labels
    (widget-value-set lire--widget-label-check-simple-past (lire--format-check-simple-past))
    (widget-value-set lire--widget-label-check-past-participle (lire--format-check-past-participle)))
  ;; Update the success layout if needed
  (lire--toggle-layout-finish)
  (setq lire--is-resolve t))

(defun lire--main-layout ()
  "Make widgets for the main layout."
  ;; Create the buffer
  (switch-to-buffer lire--buffer-name)
  ;; Clear the buffer
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  ;; Create the widgets
  ;; Title
  (insert (propertize (format "\n%s\n\n" lire--text-title) 'face '(:height 1.2 :weight bold)))
  ;; Verb in infinitive
  (setq lire--widget-item-verb (widget-create 'item
									     :value ""))
  ;; Separator
  (insert "\nSimple past     ➡️ ")
  ;; Simple past
  (setq lire--widget-field-simple-past (widget-create 'editable-field
										     :size 8
										     :help-echo "Type a Simple past"
										     ))
  ;; Label check
  (insert " ")
  (setq lire--widget-label-check-simple-past (widget-create 'item
											   (lire--format-check-simple-past)))
  ;; Separator
  (insert "\nPast participle ➡️ ")
  ;; Past participle
  (setq lire--widget-field-past-participle (widget-create 'editable-field
											 :size 8
											 :help-echo "Type a Past participle"))
  ;; Label check
  (insert " ")
  (setq lire--widget-label-check-past-participle (widget-create 'item
											       (lire--format-check-past-participle)))
  ;; Separator
  (insert "\n")
  ;; Check button
  (lire--make-button-check)
    ;; Separator
  (lire--make-space-after-check)
  ;; Show solution button
  (lire--make-button-show-solution)
  ;; Display the buffer
  (use-local-map widget-keymap)
  (widget-setup))

;; Keybindings
(define-key widget-keymap (kbd "q") 'lire--kill-app)

;; Init
(defun learning-irregular-verbs-in-English ()
  "Application to learn and review irregular verbs in English."
  (interactive)
  (lire--main-layout)
  (lire--start)
  (widget-backward 2))

(provide 'learning-irregular-verbs-in-English)

;;; learning-irregular-verbs-in-English.el ends here
