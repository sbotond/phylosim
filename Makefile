ct:
	git log --graph
cat: *.R
	rm FullSource.R;cat *.R > FullSource.R
com: *.R
	git commit -a
push:
	git push
gt:
	gitk --all
rd: *.R
	( R --vanilla < ./misc/compileman.R)
pack: *.R rd
	rm phylosim.pack/R/*.R
	rm FullSource.R
	cp *.R phylosim.pack/R/
	R CMD build phylosim.pack
clean:
	rm *.log; rm phylosim_0.1.tar.gz


