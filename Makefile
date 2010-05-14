ct:
	git log --graph
cat: *.R
	(rm FullSource.R;cat *.R > FullSource.R;true)
com: *.R
	git commit -a
push:
	git push --all
gt:
	gitk --all
rd: *.R
	( R --vanilla < ./misc/compileman.R; perl misc/RdClean.pl)
pkg: *.R rd
	(rm pkg/R/*.R;true)
	(rm FullSource.R;true)
	cp *.R pkg/R/
	cp RData/* pkg/data/
	cp PAMLdat/* pkg/data/
	R CMD build pkg
checkpkg: pkg 
	R CMD check phylosim_0.1.tar.gz
clean:
	(rm *.log; rm phylosim_0.1.tar.gz;rm -r ./phylosim.Rcheck;true)
inst: pkg
	R CMD INSTALL	phylosim_0.1.tar.gz
reinst: pkg
	R CMD REMOVE	phylosim
	R CMD INSTALL	phylosim_0.1.tar.gz
remove:
	R CMD REMOVE  phylosim
aareload: 
	(rm RData/*;true)
	R --vanilla < misc/recreate_aamodels.R

