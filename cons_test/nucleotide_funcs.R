dir<-"aln_nuc";
system(paste("mkdir ",dir));
               
# construct substitution process object
f84<-F84();
# set Kappa
f84$kappa<-f84.true.kappa;
# set base frequencies, make it GC-rich
f84$baseFreqs<-f84.true.base.freqs;

construct_root_sequence<-function(len){
	seq<-NucleotideSequence(length=len);	
        # attach process
        attachProcess(seq, f84);
	return(seq);
}

# simulate nucleotide data
simulate_nuc<-function(phylo,seq,len,rep){
	# set all states to NA
	clearStates(seq);
        # sample rates from a discrete gamma model
        plusGamma(seq,f84,shape=f84.true.gamma.shape,ncat=8);
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
        fname<-paste(dir,"/nucsim_",len,"_",rep,".fas",sep="");
     	saveAlignment(sim,file=fname,skip.internal=TRUE);
	return(fname);

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
	return(list(tmp));
}

plot_dframe<-function(d,p){
        t<-reorder(p,"pruningwise");

        for(col in names(d)){
		pl<-my_qplot(d,col);

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

		pl<-hline(pl,true);
                print(pl);
                Sys.sleep(10);
        }
}

