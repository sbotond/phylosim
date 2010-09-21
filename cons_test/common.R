# load PhyloSim
library(phylosim);
# load utility functions
source("utils.R");

# enable fast & careless mode
PSIM_FAST<-TRUE;

# load test tree
tree<-read.tree("tree/test_tree.nwk");

# function iterating over sequence length 
iterate_seq_len<-function(phylo,lengths,reps,sim.fun,est.fun){
	res<-list();	
	for(len in lengths){
		for(rep in reps){
			sim.fun(phylo,len,rep);
			res<-c(res, est.fun(phylo,len,rep));
		}
	}
	return(res);
}

