(TeX-add-style-hook
 "Exercitium_Vol1"
 (lambda ()
   (setq TeX-command-extra-options
         "-shell-escape")
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("book" "a4paper" "12pt" "twoside")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("babel" "spanish") ("xcolor" "dvipsnames") ("hyperref" "colorlinks=true" "urlcolor=blue" "pdfauthor={José A. Alonso <jalonso@us.es>}" "pdftitle={Exercitium Vol. 1}" "pdfstartview=FitH" "bookmarks=false")))
   (TeX-run-style-hooks
    "latex2e"
    "Licencia/licenciaCC"
    "book"
    "bk12"
    "tocloft"
    "fontspec"
    "xunicode"
    "xltxtra"
    "babel"
    "a4wide"
    "amssymb"
    "amsmath"
    "mathtools"
    "newunicodechar"
    "xcolor"
    "minted"
    "hyperref"
    "enumitem"
    "fancyhdr")
   (TeX-add-symbols
    '("ejercicio" 1)))
 :latex)

