#!/usr/bin/env Rscript

source("common.R");

# sequence length range
length_range<-c(10^0,10^1,10^2,10^3,10^4);

# number of replicates
reps<-1:100;

# F84 model parameters:
f84.true.kappa<-1;
f84.true.base.freqs<-c(1/6,2/6,1/6,2/6);
f84.true.gamma.shape<-0.5;

source("nucleotide_funcs.R");

res.bak<-iterate_seq_len(tree,length_range,reps,simulate_nuc,estimate_nuc);
res<-res_to_dframe(res.bak);
write.table(res,file="results/constest_nucleotide.tab");

pdf("results/constest_nuceotide.pdf");
plot_dframe(res,tree);

