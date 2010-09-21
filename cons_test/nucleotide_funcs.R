dir<-"aln_nuc";

# simulate nucleotide data
simulate_nuc<-function(phylo,len,reps){
	system(paste("mkdir ",dir));
        alns<-c();
        for(i in 1:reps){
                # construct root sequence object
                seq<-NucleotideSequence(length=len);
                # construct substitution process object
                f84<-F84();
                # set Kappa
                f84$kappa<-f84.true.kappa;
                # set base frequencies, make it GC-rich
                f84$baseFreqs<-f84.true.base.freqs;
                # attach process
                attachProcess(seq, f84);
                # sample rates from a discrete gamma model
                plusGamma(seq,f84,shape=f84.true.gamma.shape);
                # sample states
                sampleStates(seq);
                # construct simulation object
                sim<-PhyloSim(
                        phylo=phylo,
                        root.seq=seq
                );      
                # run simulation
                Simulate(sim,quiet=TRUE);
                # save alignment
                fname<-paste(dir,"/nucsim_",len,"_",i,".fas",sep="");
                saveAlignment(sim,file=fname,skip.internal=TRUE);
                alns<-c(alns, fname);
        }
                return(alns);

}

# estimate nucleotide model parameters
estimate_nuc<-function(phylo, len,rep){
	old.wd<-getwd();
	dir.name<-paste(paste("nucsim_",len,"_",rep,sep=""));
	dir.create(dir.name);
	setwd(dir.name);

	system(paste("(seqret -osf phylipnon -outseq sim.phy ../",dir,"/",dir.name,".fas 2>&1) >/dev/null",sep=""));
	system("baseml ../ctl/baseml.ctl >/dev/null");
	lines<-file.slurp("mlb");	

	res.tree<-NA;
	res.kappa<-NA;
	res.alpha<-NA;
	
	i<-1;
	while(i < length(lines)){
		if(length(grep(pattern="^tree length =",x=lines[[i]],perl=TRUE,value=FALSE)) > 0){
			res.tree<-lines[[i+4]];
			i<-i+4;
		} 
		if(length(grep(pattern="^Parameters ",x=lines[[i]],perl=TRUE,value=FALSE)) > 0){
			res.kappa<-as.numeric(lines[[i+1]]);
			i<-i+1;
		}
		if(length(grep(pattern="^alpha ",x=lines[[i]],perl=TRUE,value=FALSE)) > 0){
			res.alpha<-as.numeric(strsplit(lines[[i]],"=",fixed=TRUE)[[1]][[3]]);
			i<-i+1;
		} else {
			i<-i+1;
		}

	}

	cat(res.tree,file="res.nwk");
	res.tree<-reorder(read.tree("res.nwk"),"pruningwise");
		
	setwd(old.wd);
	system(paste("rm -fr",dir.name));
	tmp<-list();
	tmp$len=len;
	tmp$kappa=res.kappa;
	tmp$alpha=res.alpha;
	tmp$tree=res.tree;
	system(paste("rm -fr ",dir));
	return(list(tmp));
}

plot_dframe<-function(d,p){
        t<-reorder(p,"pruningwise");

        for(col in names(d)){
                pl<-qplot(data=d,as.factor(len),d[[col]],geom=c("boxplot","point"),xlab="Sequence length",ylab=col);

                true<-NA;
                if(col=="len"){
                        next();
                }
                else if (col=="kappa"){
                        true<-f84.true.kappa;
                }
                else if (col=="alpha"){
                        true<-f84.true.gamma.shape;
                }
                else { 
                        edge.nr<-as.numeric(strsplit(col,"_")[[1]][[2]]);
                        true<-t$edge.length[edge.nr];
                }

                pl<-pl + geom_abline(slope=0,intercept=true,colour="red",size=0.5);
                print(pl);
                Sys.sleep(10);
        }
}

