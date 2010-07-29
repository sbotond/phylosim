library("R.oo");
system("make cat");
source("FullSource.R");

author<-"Botond Sipos";
dest.path<-"./pkg/man";
Rdoc$compile("./FullSource.R", destPath=dest.path,verbose=TRUE);
warnings();


