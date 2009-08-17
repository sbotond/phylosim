library("R.oo");
system("make cat");
source("FullSource.R");

author<-"Botond Sipos";
dest.path<-"./man";
Rdoc$compile("./FullSource.R", destPath=dest.path,verbose=TRUE);


