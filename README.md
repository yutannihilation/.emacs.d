## Comparison w/ RStudio

| Description             | RStudio Keybind           | command                                    | ESS keybind          |
|:------------------------|:--------------------------|:-------------------------------------------|:---------------------|
| Show Function Help      | <kbd>F1</kbd>             | `ess-display-help-on-object`           | <kbd>C-c C-v</kbd>   |
| Go To Function/File     | <kbd>F2</kbd>             | `xref-find-definitions`                    | <kbd>M-.</kbd>       |
| Restart R               | <kbd>Ctrl-Shift-F10</kbd> | `inferior-ess-reload`                      | <kbd>C-c C-e r</kbd> |
| Terminate R             | -                         | `ess-cleanup`                              | <kbd>C-c C-q</kbd>   |
| Document                | <kbd>Ctrl-Shift-D</kbd>   | `ess-r-devtools-document-package`          | <kbd>C-c C-w d</kbd> |
| Load All                | <kbd>Ctrl-Shift-L</kbd>   | `ess-r-devtools-load-all`                  | <kbd>C-c C-w l</kbd> |
| Run Selected Lines      | <kbd>Ctrl-Enter</kbd>     | `ess-eval-region-or-line-and-step`         | <kbd>C-RET</kbd>     |
| Run Function Definition | <kbd>Ctrl-Alt-F</kbd>     | `ess-eval-region-or-function-or-paragraph` | <kbd>C-M-x</kbd>     |
| Find in Files           | <kbd>Ctrl-Shift-F</kbd>   | `counsel-projectile-grep`                  | <kbd>C-c p s g</kbd> |

## Tips

### Environemntal variables

While RStudio automatically cares R_LIBS_USER and PATH, we have to do it by ourselves with Emacs.

``` ini
R_LIBS_USER="~/R/library/"
PATH=/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin
```

