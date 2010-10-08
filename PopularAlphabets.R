##	
## Copyright 2009 Botond Sipos	
## See the package description for licensing information.	
##	

##
## AnyAlphabet
##
##########################################################################/** 
#
# @RdocClass AnyAlphabet
# 
# @title "The AnyAlphabet class"
# 
# \description{
#	This is a special Alphabet class which matches any alphabet. 
#	The '=='.Alphabet method always returns TRUE when one of the
#	compared objects inherits from AnyAlphabet. This behaviour is 
#	handy when creating processes that have no alphabet preference 
#	(like a deletion process).
#
#		@classhierarchy
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{...}{Not used.}
#	}
# 
# \section{Fields and Methods}{ 
# 	@allmethods
# }
# 
# \examples{ 
#	# create some alphabet objects
#	a<-BinaryAlphabet()
#	b<-NucleotideAlphabet()
#	any<-AnyAlphabet()
#	# compare objects
#	a == b
#	any == a
#	any == b
# }
# 
# @author
#
# \seealso{ 
# 	Alphabet
# }
# 
#*/###########################################################################
setConstructorS3(
  "AnyAlphabet",
  function(... ){

		this<-Alphabet(type="*ANY*",symbols=c());
		extend(this,
			"AnyAlphabet",
			 .any.flag=TRUE
			);

  },
  enforceRCC=TRUE
);

##
## BinaryAlphabet
##
##########################################################################/** 
#
# @RdocClass BinaryAlphabet
# 
# @title "The BinaryAlphabet class"
# 
# \description{ 
#	 Class of Alphabet objects with the c("0","1") symbol set.
#
#	@classhierarchy
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{...}{Not used.}
#	}
# 
# \section{Fields and Methods}{ 
# 	@allmethods
# }
# 
# \examples{ 
#	# create a binary alphabet
#	b<-BinaryAlphabet()
#	# get alphabet summary
#	summary(b)
# }
# 
# @author
#
# \seealso{ 
# 	Alphabet
# }
# 
#*/###########################################################################
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
##########################################################################/** 
#
# @RdocClass NucleotideAlphabet
# 
# @title "The NucleotideAlphabet class"
# 
# \description{ 
#	Class of Alphabet objects with the c("T","C","A","G") symbol
#	set, representing nucleotides.
#
#	@classhierarchy
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{...}{Not used.}
#	}
# 
# \section{Fields and Methods}{ 
# 	@allmethods
# }
# 
# \examples{ 
#	# create a nucleotide alphabet
#	b<-NucleotideAlphabet()
#	# get alphabet summary
#	summary(b)
# }
# 
# @author
#
# \seealso{ 
# 	Alphabet
# }
# 
#*/###########################################################################
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
##########################################################################/** 
#
# @RdocClass AminoAcidAlphabet
# 
# @title "The AminoAcidAlphabet class"
# 
# \description{ 
#	Class of Alphabet objects representing amino acids, using the
#	one-letter IUPAC amino acid codes as symbol set:
#	\preformatted{
#	IUPAC code	Amino acid
#
#	A		Alanine
#	C		Cysteine
#	D		Aspartic Acid
#	E		Glutamic Acid
#	F		Phenylalanine
#	G		Glycine
#	H		Histidine
#	I		Isoleucine
#	K		Lysine
#	L		Leucine
#	M		Methionine
#	N		Asparagine
#	P		Proline
#	Q		Glutamine
#	R		Arginine
#	S		Serine
#	T		Threonine
#	V		Valine
#	W		Tryptophan
#	Y		Tyrosine
#}
#
#	@classhierarchy
# }
#	
# @synopsis
#	
# \arguments{
# 	\item{...}{Not used.}
#	}
# 
# \section{Fields and Methods}{ 
# 	@allmethods
# }
# 
# \examples{ 
#	a<-AminoAcidAlphabet();
#	# get object summary
#	summary(a)
# }
# 
# @author
#
# \seealso{ 
# 	Alphabet
# }
# 
#*/###########################################################################
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


