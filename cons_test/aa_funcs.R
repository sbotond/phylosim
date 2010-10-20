dir<-"aln_aa";
system(paste("mkdir",dir));	
               
# construct substitution process object
wag<-WAG();

construct_root_sequence<-function(len){
        seq<-AminoAcidSequence(length=len);
	# attach process
	attachProcess(seq, wag);
	return(seq)
}

# simulate nucleotide data
simulate_aa<-function(phylo,seq,len,rep){
		# set all site states to NA
		clearStates(seq);
                # sample rates from a discrete gamma model
                plusGamma(seq,wag,shape=wag.true.gamma.shape,ncat=8);
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
                fname<-paste(dir,"/aasim_",len,"_",rep,".fas",sep="");
                saveAlignment(sim,file=fname,skip.internal=TRUE);
                return(fname);
}

# estimate nucleotide model parameters
estimate_aa<-function(phylo, len,rep){
	old.wd<-getwd();
	dir.name<-paste(paste("aasim_",len,"_",rep,sep=""));
	dir.create(dir.name);
	setwd(dir.name);

	system(paste("(seqret -osf phylipnon -outseq sim.phy ../",dir,"/",dir.name,".fas 2>&1) >/dev/null",sep=""));
	system("codeml ../ctl/codeml_aa.ctl >/dev/null");
	lines<-file.slurp("mlc");	

	res.tree<-NA;
	res.alpha<-NA;
	
	i<-1;
	while(i < length(lines)){
		if(length(grep(pattern="^tree length =",x=lines[[i]],perl=TRUE,value=FALSE)) > 0){
			res.tree<-lines[[i+4]];
			i<-i+4;
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
                else if (col=="alpha"){
                        true<-wag.true.gamma.shape;
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

