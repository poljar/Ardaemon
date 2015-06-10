filename=main

pdf:
	xelatex -shell-escape -save-size=10000 ${filename}

read: pdf
	zathura ${filename}.pdf &

clean:
	rm -f *.{pdf,log,aux,out,bbl,blg}
