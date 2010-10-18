#!/usr/bin/env Rscript

source("common.R");

# model parameters:
wag.true.gamma.shape<-1;

source("aa_funcs.R");

res.bak<-iterate_seq_len(tree,length_range,reps,simulate_aa,estimate_aa);
res<-res_to_dframe(res.bak$res);

write.table(res,file="results/constest_aa.tab");
write.table(res.bak$times,file="results/times_aa.tab");
ttab<-print_times(res.bak$times,"results/ttab_aa.tab");
save_times_latex(ttab,"ttab_aa");

pdf("results/constest_aa.pdf");
plot_dframe(res,tree)

