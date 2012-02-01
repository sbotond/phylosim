all: 
	@echo "Options: nuc aa codon cons_test"
cons_test:
	bsub -M20000 "make codon"; bsub -M20000 "make aa"; bsub -M 20000 "make nuc"
nuc:
	(R --vanilla < constest_nucleotide.R 2>&1) > results/constest_nucleotide.log
aa:
	(R --vanilla < constest_aa.R 2>&1) > results/constest_aa.log
codon:
	(R --vanilla < constest_codon.R 2>&1) > results/constest_codon.log
update_plots:
	(R --vanilla < update_plots.R 2>&1)
	
