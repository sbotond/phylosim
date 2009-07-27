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
clean:
	rm *.log


