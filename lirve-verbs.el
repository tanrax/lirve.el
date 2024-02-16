;;; lirve-verbs.el --- Verb list for learning irregular verbs in English -*- lexical-binding: t; -*-
;;
;; Copyright © 2024 Andros Fenollosa
;; Authors: Andros Fenollosa <andros@fenollosa.email>
;; URL: https://github.com/tanrax/learning-irregular-verbs-in-English.el
;; Version: 1.2.0
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; This file contains a list of irregular verbs in English, with their

;;; Code:

(defvar lirve--verbs '(
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
			 (es . "morder")))
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
			 (es . "soplar")))
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
			 (es . "oír")))
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

(provide 'lirve-verbs)
;;; lirve-verbs.el ends here
