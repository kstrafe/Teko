.PHONY:
all:
	pdflatex -halt-on-error specification.latex
	bibtex specification
	pdflatex -halt-on-error specification.latex
	bibtex specification
	pdflatex -halt-on-error specification.latex
	evince specification.pdf
