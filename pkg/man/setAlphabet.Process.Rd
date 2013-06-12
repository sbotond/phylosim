%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setAlphabet.Process}
\alias{setAlphabet.Process}
\alias{Process.setAlphabet}
\alias{setAlphabet,Process-method}

 \title{Assotiate an Alphabet object with a Process object}

 \description{
	Assotiate an Alphabet object with a Process object.
 }

 \usage{
\method{setAlphabet}{Process}(this, new.alphabet, ...)
}

 \arguments{
 	\item{this}{A Process object.}
 	\item{new.alphabet}{A valid Alphabet object.}
 	\item{...}{Not used.}
 }

 \value{
 	The Alphabet object (invisible).
 }

 \examples{
	# create objects
	a<-AminoAcidAlphabet()
	p<-Process()
	# assotiate p with Alphabet object a
	setAlphabet(p,a)
	p$alphabet
	# assotiate p with a new NucleotideAlphabet via virtual field
	p$alphabet<-NucleotideAlphabet()
	p$alphabet
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Process}}.
 }
