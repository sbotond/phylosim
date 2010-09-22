#!/usr/bin/env Rscript

source("common.R");

# sequence length range
length_range<-c(10,50,100,500,1000,5000,1000);

# number of replicates
reps<-1:100;

# GY94 model parameters:
gy94.true.omega<-2;
gy94.true.kappa<-4;

source("codon_funcs.R");

res.bak<-iterate_seq_len(tree,length_range,reps,simulate_codon,estimate_codon);
res<-res_to_dframe(res.bak);
write.table(res,file="results/constest_codon.tab");

pdf("results/constest_codon.pdf");
plot_dframe(res,tree)

