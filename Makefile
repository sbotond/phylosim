
.PHONY: ct cat com push gt checkpkg clean remove aareload 
PKG=phylosim_2.0.3.tar.gz

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
sc: *.R
	(rm -f pkg/R/*.R;true)
	cp *.R pkg/R/
rd: *.R
	( cd pkg/; R --vanilla < ../misc/compileman.R; perl ../misc/RdClean.pl)
pkg: cat sc *.R
	(rm PhyloSimSource.R;true)
	cp PAMLdat/*.dat pkg/extdata/
	cp RData/* pkg/data/
	R CMD build --compact-vignettes=both pkg
checkpkg: pkg 
	R CMD check --as-cran $(PKG)
clean:
	(rm *.log; rm $(PKG);rm -r ./phylosim.Rcheck;rm ./pkg/R/*.R;true ) 2>&1 > /dev/null
inst: pkg
	R CMD INSTALL	$(PKG)
remove:
	R CMD REMOVE  phylosim
aareload: cat
	(rm RData/*;true)
	R --vanilla < misc/recreate_aamodels.R
	R --vanilla < misc/recreate_codonmodels.R

