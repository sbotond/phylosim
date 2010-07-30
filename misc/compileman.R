library("R.oo");
system("make cat");
source("PhyloSimSource.R");

author<-"Botond Sipos";
dest.path<-"./pkg/man";
Rdoc$compile("./PhyloSimSource.R", destPath=dest.path,verbose=TRUE);
warnings();


