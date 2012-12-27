ps: 
	latex Elaborato.tex && dvips -t a4 -Ppdf Elaborato.dvi

pdf: 
	pdflatex Elaborato.tex

