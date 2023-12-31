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
	(verbs-learned '())
	(color-valid "green")
	(color-error "red")
	(widget-item-first-verb nil)
	(widget-editable-first-simple-past nil)
	(widget-editable-first-past-participle nil)
	(widget-button-first)
	))
  )

(provide 'learning-irregular-verbs-in-English)

;;; learning-irregular-verbs-in-English.el ends here
