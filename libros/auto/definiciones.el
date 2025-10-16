(TeX-add-style-hook
 "definiciones"
 (lambda ()
   (setq TeX-command-extra-options
         "-shell-escape")
   (TeX-add-symbols
    '("ejercicio" 1))
   (LaTeX-add-environments
    "descripcion"
    "code"))
 :latex)

