##	$Id: PopularAlphabets.R,v 1.2 2009-05-01 16:06:14 sbotond Exp $
##
##	Class: *
##	Descriprion: 
##	
##	
##	
##	
##	
##	
##	
## Copyright 2009 Botond Sipos	
## See the file ../COPYING for licensing issues.	
##	

##
## BinaryAlphabet
##
setConstructorS3(
  "BinaryAlphabet",
  function(... ){

		this<-Alphabet(type="Binary",symbols=c("0","1"));
		extend(this,"BinaryAlphabet");

  },
  enforceRCC=TRUE
);

##
## NucleotideAlphabet
##
setConstructorS3(
  "NucleotideAlphabet",
  function(... ){

		this<-Alphabet(type="Nucleotide",symbols=c("T","C","A","G"));
		extend(this,"NucleotideAlphabet");

  },
  enforceRCC=TRUE
);

##
## AminoAcidAlphabet
##
setConstructorS3(
  "AminoAcidAlphabet",
  function(... ){

		this<-Alphabet(
				type="Amino acid",
				symbols=c(
					"A",
					"R",
					"N",
					"D", 
					"C",
					"Q",
					"E",
					"G", 
					"H",
					"I",
					"L",
					"K",
					"M",
					"F",
					"P",
					"S", 
					"T",
					"W",
					"Y",
					"V"
				)
		 );
		extend(this,"AminoAcidAlphabet");

  },
  enforceRCC=TRUE
);


