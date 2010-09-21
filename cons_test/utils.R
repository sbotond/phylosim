file.slurp<-function(file){
	scan(file=file,what="character",sep="\n",blank.lines.skip=FALSE,quiet=TRUE);
}

res_to_dframe<-function(l){
        d<-list();
        for(i in l){   
                        for(name in names(i)){
                                if(class(i[[name]]) != "phylo"){
                                        d[[name]]<-c(d[[name]],i[[name]]);
                                } else {
                                        p<-i[[name]];
                                        i<-1;
                                        for(el in p$edge.length){
                                                edge.name<-paste("edge",i,sep="_");
                                                d[[edge.name]]<-c(d[[edge.name]],el);
                                                i<-i+1;
                                        }

                                }
                        }
        }

        d<-as.data.frame(d);
        return(d);
}


