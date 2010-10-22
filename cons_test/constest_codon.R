#!/usr/bin/env Rscript

source("common.R");

# GY94 model parameters:
gy94.true.omega<-0.3;
gy94.true.kappa<-4;

source("codon_funcs.R");

# Calculate PAML-style branch length scaling factor:
sf<-getOmegaScalingFactor(gy94,gy94.true.omega);

res.bak<-iterate_seq_len(tree,length_range,reps,simulate_codon,estimate_codon);
res<-res_to_dframe(res.bak$res);

write.table(res,file="results/constest_codon.tab");
write.table(res.bak$times,file="results/times_codon.tab");
ttab<-print_times(res.bak$times,"results/ttab_codon.tab");
save_times_latex(ttab,"ttab_codon");

pdf("results/constest_codon.pdf");
plot_dframe(res,tree)

