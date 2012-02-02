# load PhyloSim
library(phylosim);

# get CPU info:
system("cat /proc/cpuinfo");

# load utility functions
source("utils.R");

length_range<-c(10, 50, 100, 500, 1000, 5000, 10000);
reps<-1:100;

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
		for(rep in reps){
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
	}

	return(list("res"=res,"times"=as.data.frame(times)));
}

# function to create ggplot objects
my_qplot<-function(d,col){
	qplot(data=d,as.factor(len),d[[col]],geom=c("jitter"),h=0,xlab="Sequence length",ylab=col,log="y",size=0.7) + geom_boxplot(size=0.3,colour="blue",fill="blue", alpha=0.3/3,outlier.size=0.3,coef=100000) + opts(legend.position = "none");
}

# function to draw horizontal line
hline<-function(pl,true){
	pl<-pl + geom_abline(slope=0,intercept=log(true,base=10),colour="red",size=0.1);
	pl;
}
