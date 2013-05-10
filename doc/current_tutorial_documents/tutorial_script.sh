dot -Tpng < dotInput > dotInput.png
dot -Tpng < dotFields > dotFields.png
dot -Tpng < dotChecks > dotChecks.png
dot -Tpng < dotRecodes > dotRecodes.png
dot -Tpng < dotImpute > dotImpute.png

pdflatex tutorial.tex
