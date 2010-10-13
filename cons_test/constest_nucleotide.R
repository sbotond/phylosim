#!/usr/bin/env Rscript

source("common.R");

# F84 model parameters:
f84.true.kappa<-1;
f84.true.base.freqs<-c(1.5/6,1.25/6,1.5/6,1.25/6);
f84.true.gamma.shape<-0.5;

source("nucleotide_funcs.R");

res.bak<-iterate_seq_len(tree,length_range,reps,simulate_nuc,estimate_nuc);
res<-res_to_dframe(res.bak$res);

write.table(res,file="results/constest_nucleotide.tab");
write.table(res.bak$times,file="results/times_nucleotide.tab");
print_times(res.bak$times,"results/ttab_nucleotide.tab");

pdf("results/constest_nucleotide.pdf");
plot_dframe(res,tree);

