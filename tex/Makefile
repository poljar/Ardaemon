filename=main

pdf:
	xelatex ${filename}

read: pdf
	zathura ${filename}.pdf &

clean:
	rm -f ${filename}.{pdf,log,aux,out,bbl,blg}
