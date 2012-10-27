ps: 
	latex Elaborato.tex && dvips -t a4 -Ppdf Elaborato.dvi

pdf: ps
	ps2pdf Elaborato.ps Elaborato.pdf

