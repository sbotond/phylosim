
.PHONY: ct cat com push gt checkpkg clean remove aareload
PKG=phylosim_0.15.tar.gz

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
	R CMD build pkg
checkpkg: pkg 
	R CMD check $(PKG)
clean:
	(rm *.log; rm $(PKG);rm -r ./phylosim.Rcheck;rm ./pkg/man/*.Rd;rm ./pkg/R/*.R;true ) 2>&1 > /dev/null
inst: pkg
	R CMD INSTALL	$(PKG)
remove:
	R CMD REMOVE  phylosim
aareload: cat
	(rm RData/*;true)
	R --vanilla < misc/recreate_aamodels.R

