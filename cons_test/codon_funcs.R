dir<-"aln_codon";

# simulate nucleotide data
simulate_codon<-function(phylo,len,reps){
	# create directories
	system(paste("mkdir",dir));	
        alns<-c();
        for(i in 1:reps){
                # construct root sequence object
                seq<-CodonSequence(length=len);
                # construct substitution process object
                gy94<-GY94(kappa=gy94.true.kappa);
                # attach process
                attachProcess(seq, gy94);
                # set omega
		omegaVarM0(seq,gy94,omega=gy94.true.omega);
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
                fname<-paste(dir,"/codonsim_",len,"_",i,".fas",sep="");
                saveAlignment(sim,file=fname,skip.internal=TRUE);
                alns<-c(alns, fname);
        }
                return(alns);

}

# estimate nucleotide model parameters
estimate_codon<-function(phylo, len,rep){
	old.wd<-getwd();
	dir.name<-paste(paste("codonsim_",len,"_",rep,sep=""));
	dir.create(dir.name);
	setwd(dir.name);

	system(paste("(seqret -osf phylipnon -outseq sim.phy ../",dir,"/",dir.name,".fas 2>&1) >/dev/null",sep=""));
	system("codeml ../ctl/codeml_codon.ctl >/dev/null");
	lines<-file.slurp("mlc");	

	res.tree<-NA;
	res.alpha<-NA;
	
	i<-1;
	while(i < length(lines)){
		if(length(grep(pattern="^tree length =",x=lines[[i]],perl=TRUE,value=FALSE)) > 0){
			res.tree<-lines[[i+4]];
			i<-i+4;
		} 
		else if(length(grep(pattern="^kappa ",x=lines[[i]],perl=TRUE,value=FALSE)) > 0){
			res.kappa<-as.numeric(strsplit(lines[[i]],"=",fixed=TRUE)[[1]][[2]]);
			i<-i+1;
		} 
		else if(length(grep(pattern="^omega ",x=lines[[i]],perl=TRUE,value=FALSE)) > 0){
			res.omega<-as.numeric(strsplit(lines[[i]],"=",fixed=TRUE)[[1]][[2]]);
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
	tmp$omega=res.omega;
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
                        true<-gy94.true.kappa;
                }
                else if (col=="omega"){
                        true<-gy94.true.omega;
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

