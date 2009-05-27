#$Id: Makefile,v 1.3 2009-03-30 15:36:53 sbotond Exp $
cat: *.R
	rm FullSource.R;cat *.R > FullSource.R
com: *.R
	rm FullSource.R;cat *.R > FullSource.R ;git commit -a
push:
	git push linear master	
gt:
	gitk --all


