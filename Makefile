TEXFILES  = Haochi_Kiang.tex
NAME      = Haochi_Kiang

all: pdf

pdf: $(NAME).pdf

%.pdf:
	pdflatex $(TEXFILES) && pdflatex $(TEXFILES) && pdflatex $(TEXFILES)

clean:
	rm -f $(NAME).log $(NAME).aux $(NAME).bbl $(NAME).synctex.gz $(NAME).pdf

