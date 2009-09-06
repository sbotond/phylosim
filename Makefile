ct:
	git log --graph
cat: *.R
	(rm FullSource.R;cat *.R > FullSource.R;true)
com: *.R
	git commit -a
push:
	git push
gt:
	gitk --all
rd: *.R
	( R --vanilla < ./misc/compileman.R)
pack: *.R rd
	(rm pack/R/*.R;true)
	(rm FullSource.R;true)
	cp *.R pack/R/
	cp RData/* pack/data/
	cp PAMLdat/* pack/data/
	R CMD build pack
checkpack: pack 
	R CMD check phylosim_0.1.tar.gz
clean:
	(rm *.log; rm phylosim_0.1.tar.gz;true)
inst: pack
	R CMD INSTALL	phylosim_0.1.tar.gz
reinst: pack
	R CMD REMOVE	phylosim
	R CMD INSTALL	phylosim_0.1.tar.gz
remove:
	R CMD REMOVE  phylosim
aareload: 
	(rm RData/*;true)
	R --vanilla < misc/recreate_aamodels.R

