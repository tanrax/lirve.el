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

(defvar  lire--verbs '(
		      (
		       (infinitive . "beat")
		       (simple-past . "beat")
		       (past-participle . "beaten")
		       (translations
			(es . "golpear")))
		      (
		       (infinitive . "become")
		       (simple-past . "became")
		       (past-participle . "become")
		       (translations
			(es . "llegar a ser, convertirse en")))
		      (
		       (infinitive . "begin")
		       (simple-past . "began")
		       (past-participle . "begun")
		       (translations
			(es . "empezar")))
		      (
		       (infinitive . "bend")
		       (simple-past . "bent")
		       (past-participle . "bent")
		       (translations
			(es . "doblar")))
		      (
		       (infinitive . "bet")
		       (simple-past . "bet")
		       (past-participle . "bet")
		       (translations
			(es . "apostar")))
		      (
		       (infinitive . "bite")
		       (simple-past . "bit")
		       (past-participle . "bitten")
		       (translations
			(es . ("morder"))))
		      (
		       (infinitive . "bleed")
		       (simple-past . "bled")
		       (past-participle . "bled")
		       (translations
			(es . "sangrar")))
		      (
		       (infinitive . "blow")
		       (simple-past . "blew")
		       (past-participle . "blown")
		       (translations
			(es . ("soplar"))))
		      (
		       (infinitive . "break")
		       (simple-past . "broke")
		       (past-participle . "broken")
		       (translations
			(es . "romper")))
		      (
		       (infinitive . "bring")
		       (simple-past . "brought")
		       (past-participle . "brought")
		       (translations
			(es . "traer")))
		      (
		       (infinitive . "build")
		       (simple-past . "built")
		       (past-participle . "built")
		       (translations
			(es . "construir")))
		      (
		       (infinitive . "burn")
		       (simple-past . "burnt")
		       (past-participle . "burnt")
		       (translations
			(es . "quemar")))
		      (
		       (infinitive . "buy")
		       (simple-past . "bought")
		       (past-participle . "bought")
		       (translations
			(es . "comprar")))
		      (
		       (infinitive . "catch")
		       (simple-past . "caught")
		       (past-participle . "caught")
		       (translations
			(es . "coger")))
		      (
		       (infinitive . "choose")
		       (simple-past . "chose")
		       (past-participle . "chosen")
		       (translations
			(es . "escoger")))
		      (
		       (infinitive . "come")
		       (simple-past . "came")
		       (past-participle . "come")
		       (translations
			(es . "venir")))
		      (
		       (infinitive . "cost")
		       (simple-past . "cost")
		       (past-participle . "cost")
		       (translations
			(es . "costar")))
		      (
		       (infinitive . "cut")
		       (simple-past . "cut")
		       (past-participle . "cut")
		       (translations
			(es . "cortar")))
		      (
		       (infinitive . "dig")
		       (simple-past . "dug")
		       (past-participle . "dug")
		       (translations
			(es . "cavar")))
		      (
		       (infinitive . "do")
		       (simple-past . "did")
		       (past-participle . "done")
		       (translations
			(es . "hacer")))
		      (
		       (infinitive . "draw")
		       (simple-past . "drew")
		       (past-participle . "drawn")
		       (translations
			(es . '"(dibujar, trazar")))
		      (
		       (infinitive . "dream")
		       (simple-past . "dreamt")
		       (past-participle . "dreamt")
		       (translations
			(es . "soñar")))
		      (
		       (infinitive . "drink")
		       (simple-past . "drank")
		       (past-participle . "drunk")
		       (translations
			(es . "beber")))
		      (
		       (infinitive . "drive")
		       (simple-past . "drove")
		       (past-participle . "driven")
		       (translations
			(es . "conducir")))
		      (
		       (infinitive . "eat")
		       (simple-past . "ate")
		       (past-participle . "eaten")
		       (translations
			(es . "comer")))
		      (
		       (infinitive . "fall")
		       (simple-past . "fell")
		       (past-participle . "fallen")
		       (translations
			(es . "caer(se)")))
		      (
		       (infinitive . "feed")
		       (simple-past . "fed")
		       (past-participle . "fed")
		       (translations
			(es . "dar de comer, alimentar")))
		      (
		       (infinitive . "feel")
		       (simple-past . "felt")
		       (past-participle . "felt")
		       (translations
			(es . "sentir")))
		      (
		       (infinitive . "fight")
		       (simple-past . "fought")
		       (past-participle . "fought")
		       (translations
			(es . "pelear, luchar")))
		      (
		       (infinitive . "find")
		       (simple-past . "found")
		       (past-participle . "found")
		       (translations
			(es . "encontrar")))
		      (
		       (infinitive . "fly")
		       (simple-past . "flew")
		       (past-participle . "flown")
		       (translations
			(es . "volar")))
		      (
		       (infinitive . "forget")
		       (simple-past . "forgot")
		       (past-participle . "forgotten")
		       (translations
			(es . "olvidar")))
		      (
		       (infinitive . "forgive")
		       (simple-past . "forgave")
		       (past-participle . "forgiven")
		       (translations
			(es . "perdonar")))
		      (
		       (infinitive . "freeze")
		       (simple-past . "froze")
		       (past-participle . "frozen")
		       (translations
			(es . "helar, congelar")))
		      (
		       (infinitive . "get")
		       (simple-past . "got")
		       (past-participle . "got")
		       (translations
			(es . "conseguir")))
		      (
		       (infinitive . "give")
		       (simple-past . "gave")
		       (past-participle . "given")
		       (translations
			(es . "dar")))
		      (
		       (infinitive . "go")
		       (simple-past . "went")
		       (past-participle . "gone")
		       (translations
			(es . "ir")))
		      (
		       (infinitive . "grow")
		       (simple-past . "grew")
		       (past-participle . "grown")
		       (translations
			(es . "cultivar, crecer")))
		      (
		       (infinitive . "hang")
		       (simple-past . "hung")
		       (past-participle . "hung")
		       (translations
			(es . "colgar")))
		      (
		       (infinitive . "have")
		       (simple-past . "had")
		       (past-participle . "had")
		       (translations
			(es . "tener")))
		      (
		       (infinitive . "hear")
		       (simple-past . "heard")
		       (past-participle . "heard")
		       (translations
			(es .' ("oír"))))
		      (
		       (infinitive . "hide")
		       (simple-past . "hid")
		       (past-participle . "hidden")
		       (translations
			(es . "esconder")))
		      (
		       (infinitive . "hit")
		       (simple-past . "hit")
		       (past-participle . "hit")
		       (translations
			(es . "golpear, pegar")))
		      (
		       (infinitive . "hold")
		       (simple-past . "held")
		       (past-participle . "held")
		       (translations
			(es . "sostener")))
		      (
		       (infinitive . "hurt")
		       (simple-past . "hurt")
		       (past-participle . "hurt")
		       (translations
			(es . "herir")))
		      (
		       (infinitive . "keep")
		       (simple-past . "kept")
		       (past-participle . "kept")
		       (translations
			(es . "mantener")))
		      (
		       (infinitive . "know")
		       (simple-past . "knew")
		       (past-participle . "known")
		       (translations
			(es . "saber, conocer")))
		      (
		       (infinitive . "lay")
		       (simple-past . "laid")
		       (past-participle . "laid")
		       (translations
			(es . "poner (la mesa), colocar")))
		      (
		       (infinitive . "lead")
		       (simple-past . "led")
		       (past-participle . "led")
		       (translations
			(es . "dirigir")))
		      (
		       (infinitive . "learn")
		       (simple-past . "learnt")
		       (past-participle . "learnt")
		       (translations
			(es . "aprender")))
		      (
		       (infinitive . "leave")
		       (simple-past . "left")
		       (past-participle . "left")
		       (translations
			(es . "dejar, marcharse(se)")))
		      (
		       (infinitive . "lend")
		       (simple-past . "lent")
		       (past-participle . "lent")
		       (translations
			(es . "prestar")))
		      (
		       (infinitive . "let")
		       (simple-past . "let")
		       (past-participle . "let")
		       (translations
			(es . "dejar, permitir")))
		      (
		       (infinitive . "lie")
		       (simple-past . "lay")
		       (past-participle . "lain")
		       (translations
			(es . "tumbarse")))
		      (
		       (infinitive . "light")
		       (simple-past . "lit")
		       (past-participle . "lit")
		       (translations
			(es . "iluminar")))
		      (
		       (infinitive . "lose")
		       (simple-past . "lost")
		       (past-participle . "lost")
		       (translations
			(es . "perder")))
		      (
		       (infinitive . "make")
		       (simple-past . "made")
		       (past-participle . "made")
		       (translations
			(es . "hacer")))
		      (
		       (infinitive . "mean")
		       (simple-past . "meant")
		       (past-participle . "meant")
		       (translations
			(es . "significar, querer decir")))
		      (
		       (infinitive . "meet")
		       (simple-past . "met")
		       (past-participle . "met")
		       (translations
			(es . "conocer")))
		      (
		       (infinitive . "pay")
		       (simple-past . "paid")
		       (past-participle . "paid")
		       (translations
			(es . "pagar")))
		      (
		       (infinitive . "put")
		       (simple-past . "put")
		       (past-participle . "put")
		       (translations
			(es . "poner")))
		      (
		       (infinitive . "read")
		       (simple-past . "read")
		       (past-participle . "read")
		       (translations
			(es . "leer")))
		      (
		       (infinitive . "ride")
		       (simple-past . "rode")
		       (past-participle . "ridden")
		       (translations
			(es . "montar")))
		      (
		       (infinitive . "ring")
		       (simple-past . "rang")
		       (past-participle . "rung")
		       (translations
			(es . "sonar, llamar por teléfono")))
		      (
		       (infinitive . "rise")
		       (simple-past . "rose")
		       (past-participle . "risen")
		       (translations
			(es . "levantarse")))
		      (
		       (infinitive . "run")
		       (simple-past . "ran")
		       (past-participle . "run")
		       (translations
			(es . "correr")))
		      (
		       (infinitive . "say")
		       (simple-past . "said")
		       (past-participle . "said")
		       (translations
			(es . "decir")))
		      (
		       (infinitive . "see")
		       (simple-past . "saw")
		       (past-participle . "seen")
		       (translations
			(es . "ver")))
		      (
		       (infinitive . "sell")
		       (simple-past . "sold")
		       (past-participle . "sold")
		       (translations
			(es . "vender")))
		      (
		       (infinitive . "send")
		       (simple-past . "sent")
		       (past-participle . "sent")
		       (translations
			(es . "enviar")))
		      (
		       (infinitive . "set")
		       (simple-past . "set")
		       (past-participle . "set")
		       (translations
			(es . "colocar, fijar, poner la mesa")))
		      (
		       (infinitive . "shake")
		       (simple-past . "shook")
		       (past-participle . "shaken")
		       (translations
			(es . "agitar, sacudir")))
		      (
		       (infinitive . "shine")
		       (simple-past . "shone")
		       (past-participle . "shone")
		       (translations
			(es . "brillar, sacar brillo")))
		      (
		       (infinitive . "shoot")
		       (simple-past . "shot")
		       (past-participle . "shot")
		       (translations
			(es . "disparar")))
		      (
		       (infinitive . "show")
		       (simple-past . "showed")
		       (past-participle . "shown")
		       (translations
			(es . "mostrar")))
		      (
		       (infinitive . "shut")
		       (simple-past . "shut")
		       (past-participle . "shut")
		       (translations
			(es . "cerrar")))
		      (
		       (infinitive . "sing")
		       (simple-past . "sang")
		       (past-participle . "sung")
		       (translations
			(es . "cantar")))
		      (
		       (infinitive . "sink")
		       (simple-past . "sank")
		       (past-participle . "sunk")
		       (translations
			(es . "hundir(se)")))
		      (
		       (infinitive . "sit")
		       (simple-past . "sat")
		       (past-participle . "sat")
		       (translations
			(es . "sentar(se)")))
		      (
		       (infinitive . "sleep")
		       (simple-past . "slept")
		       (past-participle . "slept")
		       (translations
			(es . "dormir")))
		      (
		       (infinitive . "smell")
		       (simple-past . "smelt")
		       (past-participle . "smelt")
		       (translations
			(es . "oler")))
		      (
		       (infinitive . "speak")
		       (simple-past . "spoke")
		       (past-participle . "spoken")
		       (translations
			(es . "hablar")))
		      (
		       (infinitive . "spell")
		       (simple-past . "spelt")
		       (past-participle . "spelt")
		       (translations
			(es . "deletrear")))
		      (
		       (infinitive . "spend")
		       (simple-past . "spent")
		       (past-participle . "spent")
		       (translations
			(es . "gastar")))
		      (
		       (infinitive . "spill")
		       (simple-past . "spilt")
		       (past-participle . "spilt")
		       (translations
			(es . "derramar")))
		      (
		       (infinitive . "spit")
		       (simple-past . "spit")
		       (past-participle . "spit")
		       (translations
			(es . "escupir")))
		      (
		       (infinitive . "stand")
		       (simple-past . "stood")
		       (past-participle . "stood")
		       (translations
			(es . "ponerse/estar de pie")))
		      (
		       (infinitive . "steal")
		       (simple-past . "stole")
		       (past-participle . "stolen")
		       (translations
			(es . "robar")))
		      (
		       (infinitive . "swim")
		       (simple-past . "swam")
		       (past-participle . "swum")
		       (translations
			(es . "nadar")))
		      (
		       (infinitive . "take")
		       (simple-past . "took")
		       (past-participle . "taken")
		       (translations
			(es . "tomar, coger")))
		      (
		       (infinitive . "teach")
		       (simple-past . "taught")
		       (past-participle . "taught")
		       (translations
			(es . "enseñar")))
		      (
		       (infinitive . "tear")
		       (simple-past . "tore")
		       (past-participle . "torn")
		       (translations
			(es . "romper, rasgar")))
		      (
		       (infinitive . "tell")
		       (simple-past . "told")
		       (past-participle . "told")
		       (translations
			(es . "decir, contar")))
		      (
		       (infinitive . "think")
		       (simple-past . "thought")
		       (past-participle . "thought")
		       (translations
			(es . "pensar")))
		      (
		       (infinitive . "throw")
		       (simple-past . "threw")
		       (past-participle . "thrown")
		       (translations
			(es . "lanzar, tirar")))
		      (
		       (infinitive . "understand")
		       (simple-past . "understood")
		       (past-participle . "understood")
		       (translations
			(es . "entender")))
		      (
		       (infinitive . "wake")
		       (simple-past . "woke")
		       (past-participle . "woken")
		       (translations
			(es . "despertar")))
		      (
		       (infinitive . "wear")
		       (simple-past . "wore")
		       (past-participle . "worn")
		       (translations
			(es . "llevar puesto")))
		      (
		       (infinitive . "win")
		       (simple-past . "won")
		       (past-participle . "won")
		       (translations
			(es . "ganar")))
		      (
		       (infinitive . "write")
		       (simple-past . "wrote")
		       (past-participle . "written")
		       (translations
			(es . "escribir")))))
;; Variables
(defvar lire--buffer-name "*Learning irregular verbs in English*")
(defvar lire--state 1) ;; 1: lire--start, 2: playing (before first check), 3: win (show success layout)
(defvar lire--verb-to-learn-infinitive nil)
(defvar lire--verb-to-learn-simple-past nil)
(defvar lire--verb-to-learn-past-participle nil)
(defvar lire--translation nil)
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
    (setq lire--verb-to-learn-infinitive (alist-get 'infinitive verbs-random))
    (setq lire--verb-to-learn-simple-past (alist-get 'simple-past verbs-random))
    (setq lire--verb-to-learn-past-participle (alist-get 'past-participle verbs-random))
    (when (not (null (boundp 'learning-irregular-verbs-in-English--show-translation))) (setq lire--translation (alist-get learning-irregular-verbs-in-English--show-translation (alist-get 'translations verbs-random))))))

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

(defun lire--show-translation ()
  "Show translation if learning-irregular-verbs-in-English--show-translation is t"
  (when (not (null lire--translation))
    (widget-value-set lire--widget-item-verb (concat (lire--format-value-infinitive) "   🇪🇸 " lire--translation))))

(defun lire--toggle-layout-finish ()
  "Toggle the layout to success."
  (if (eq lire--state 3)
      (progn
	;; Show translate
	(lire--show-translation)
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

;; Init
(defun learning-irregular-verbs-in-English ()
  "Application to learn and review irregular verbs in English."
  (interactive)
  (lire--main-layout)
  (lire--start)
  (widget-backward 4))

(provide 'learning-irregular-verbs-in-English)

;;; learning-irregular-verbs-in-English.el ends here
