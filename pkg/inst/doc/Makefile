all: PhyloSim.tex
	pdflatex PhyloSim.tex;
	pdflatex PhyloSim.tex;
	rm PhyloSim.aux
	rm PhyloSim.log
	rm PhyloSim.toc
	rm PhyloSim.tex
sw:
	R CMD Sweave PhyloSim.Rnw
clean:
	rm PhyloSim.pdf
	
