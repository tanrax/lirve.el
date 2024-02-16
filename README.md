# Lirve: Learn irregular English verbs in Emacs

Lirve helps you learn irregular verbs using the spaced repetition technique. In other words: Lirve remember your mistakes and repeat the challenge in the future.

![Demo](demo.png)

## Install

You need to have `straight` installed.

Then, add it to your `init.el`.

```elisp
(use-package learning-irregular-verbs-in-english
  :straight (:host github :repo "tanrax/learning-irregular-verbs-in-english.el" :files ("lirve-verbs.el" "lirve.el"))
  :ensure t)
```

And add the following to your `init.el`:

```elisp
(require 'lirve)
```

## Configure (Optional)

Shows the translation of the verb when resolving or failing.

![Demo translation](demo-translation.png)

Only available in Spanish (at the moment).

```elisp
(setq lirve--show-translation 'es)
```

## Usage

```
M-x learning-irregular-verbs-in-english
```

## Controls

| Key | Description |
| --- | --- |
| `TAB` | Move to the next field |
| `S-TAB` | Move to the previous field |
| `RET` | Click on the button |

## Collaborate

If you want to add more languages, make a PR with the translations in `lirve-verbs.el`.

For example, the verb `beat` in Italian and Spanish:

```ellisp
(
    (infinitive . "beat")
    (simple-past . "beat")
    (past-participle . "beaten")
    (translations
        (es . "golpear")
        (it . "colpo")))
```
