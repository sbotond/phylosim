
.PHONY: ct cat com push gt checkpkg clean remove aareload

ct:
	git log --graph
cat: *.R
	(rm PhyloSimSource.R;cat *.R > PhyloSimSource.R;true)
com: *.R
	git commit -a
push:
	git push --all
fetch:
	git fetch --all
gt:
	gitk --all
rd: *.R
	( R --vanilla < ./misc/compileman.R; perl misc/RdClean.pl)
pkg: *.R rd cat
	(rm pkg/R/*.R;true)
	(rm PhyloSimSource.R;true)
	cp *.R pkg/R/
	cp RData/* pkg/data/
	cp PAMLdat/* pkg/data/
	R CMD build pkg
checkpkg: pkg 
	R CMD check phylosim_0.11.tar.gz
clean:
	(rm *.log; rm phylosim_0.11.tar.gz;rm -r ./phylosim.Rcheck;rm ./pkg/man/*.Rd;rm ./pkg/R/*.R;true ) 2>&1 > /dev/null
inst: pkg
	R CMD INSTALL	phylosim_0.11.tar.gz
reinst: pkg
	R CMD REMOVE	phylosim
	R CMD INSTALL	phylosim_0.11.tar.gz
remove:
	R CMD REMOVE  phylosim
aareload: cat
	(rm RData/*;true)
	R --vanilla < misc/recreate_aamodels.R

