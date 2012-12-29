pdf: 
	pdflatex Elaborato.tex
ps: 
	latex Elaborato.tex && dvips -t a4 -Ppdf Elaborato.dvi


