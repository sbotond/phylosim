all: PhyloSim.tex
	pdflatex PhyloSim.tex;
	pdflatex PhyloSim.tex;
	rm PhyloSim.aux
	rm PhyloSim.log
	rm PhyloSim.toc
	rm PhyloSim.out
	rm PhyloSim.tex
	rm *.eps
	rm PhyloSim-*.pdf
	rm Rplots.pdf
sw:
	R CMD Sweave PhyloSim.Rnw
clean:
	rm PhyloSim.pdf
v:
	okular PhyloSim.pdf
	
