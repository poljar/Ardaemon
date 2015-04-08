filename=slides

pdf:
	xelatex -shell-escape ${filename}

read: pdf
	evince ${filename}.pdf &

clean:
	rm -f ${filename}.{pdf,log,aux,out,bbl,blg,toc,snm,nav,vrb,pyg}
