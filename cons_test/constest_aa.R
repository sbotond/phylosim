#!/usr/bin/env Rscript

source("common.R");

# sequence length range
length_range<-c(10^0,10^1,10^2,10^3,10^4);

# number of replicates
reps<-1:100;

# model parameters:
wag.true.gamma.shape<-0.1;

source("aa_funcs.R");

res.bak<-iterate_seq_len(tree,length_range,reps,simulate_aa,estimate_aa);
res<-res_to_dframe(res.bak);
write.table(res,file="results/constest_aa.tab");

pdf("results/constest_aa.pdf");
plot_dframe(res,tree)

