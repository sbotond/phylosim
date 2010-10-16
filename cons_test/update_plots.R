#!/usr/bin/env Rscript

source("common.R");


# F84 model parameters:
f84.true.kappa<-1;
f84.true.base.freqs<-c(1.5/6,1.25/6,1.5/6,1.25/6);
f84.true.gamma.shape<-0.5;

# model parameters:
wag.true.gamma.shape<-0.1;

# GY94 model parameters:
gy94.true.omega<-2;
gy94.true.kappa<-4;


source("nucleotide_funcs.R");
res<-read.table(file="results/constest_nucleotide.tab");
pdf("results/constest_nucleotide.pdf");
plot_dframe(res,tree);
dev.off();

source("aa_funcs.R");
res<-read.table(file="results/constest_aa.tab");
pdf("results/constest_aa.pdf");
plot_dframe(res,tree);
dev.off();

source("codon_funcs.R");
res<-read.table(file="results/constest_codon.tab");
pdf("results/constest_codon.pdf");
plot_dframe(res,tree);
dev.off();




