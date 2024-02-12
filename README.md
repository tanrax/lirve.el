# Learn irregular English verbs in Emacs
## learning-irregular-verbs-in-english.el

![Demo](demo.png)

## Install

Add in your `init.el`.

```elisp
(use-package learning-irregular-verbs-in-english
  :straight (:host github :repo "tanrax/learning-irregular-verbs-in-English.el" :files ("lirve--verbs.el" "lirve.el"))
  :ensure t)
```

## Configure (Optional)

Shows the translation of the verb when resolving or failing.

![Demo translation](demo-translation.png)

Only available in Spanish.

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
| `q` | Quit |
