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

print_times<-function(d,f){
	sim<-xtabs(data=d,sim ~ len);	
	est<-xtabs(data=d,est ~ len);	
	tab<-rbind(sim,est)/length(reps);
	write.table(tab,file=f);
	tab;
}

save_times_latex<-function(d,name){
        d<-(round(d,digits=3));

        row1.sim<-as.character(d[1,]);

        row1.est<-as.character(d[2,]);

        ts<-paste(c("Simulation",row1.sim),collapse="& ");
        ts<-paste(ts,"\\\\\n",sep="");

        te<-paste(c("Estimation",row1.est),collapse="& ",sep="");
        te<-paste(te,"\\\\\n",sep="");

        cat(ts,file=paste("results/",name,"_r1.tex",sep=""));
        cat(te,file=paste("results/",name,"_r1.tex",sep=""),append=TRUE);

        return(TRUE);
}

