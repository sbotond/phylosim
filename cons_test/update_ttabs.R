#!/usr/bin/env Rscript

source("utils.R");

ttab<-read.table("results/ttab_nucleotide.tab");                                                   
save_times_latex(ttab,"ttab_nucleotide");

ttab<-read.table("results/ttab_aa.tab");                                                   
save_times_latex(ttab,"ttab_aa");

ttab<-read.table("results/ttab_codon.tab");                                                   
save_times_latex(ttab,"ttab_codon");





