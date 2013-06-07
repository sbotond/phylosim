library("R.oo");
system("make cat");

dest.path<-"./man";
sf       <-list.files()
Rdoc$compile(filename="../PhyloSimSource.R", destPath=dest.path,verbose=TRUE, source=TRUE);
warnings();


