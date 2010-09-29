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
	times<-list(len=numeric(),rep=numeric(),sim=numeric(),est=numeric(),el.sim=numeric(),el.est=numeric());

	for(len in lengths){
			seq<-construct_root_sequence(len);
		for(rep in reps)
			sim.t<-system.time(sim.fun(phylo,seq,len,rep));
			est.t<-system.time(est<-est.fun(phylo,len,rep));
			res<-c(res, est);

			# paste timing info:
			times$len<-c(times$len,len);
			times$rep<-c(times$rep,rep);
			times$sim<-c(times$sim,(as.numeric(sim.t[1])+as.numeric(sim.t[2])));
			times$est<-c(times$est,(as.numeric(est.t[1])+as.numeric(est.t[2])));
			times$el.sim<-c(times$el.sim,as.numeric(sim.t[3]));
			times$el.est<-c(times$el.est,as.numeric(est.t[3]));
	}

	return(list("res"=res,"times"=as.data.frame(times)));
}

