ct:
	git log --graph
cat: *.R
	rm FullSource.R;cat *.R > FullSource.R
com: *.R
	rm FullSource.R;cat *.R > FullSource.R ;git commit -a
push:
	git push
gt:
	gitk --all
rd: *.R
	( R --vanilla < ./misc/compileman.R)
clean:
	rm *.log


