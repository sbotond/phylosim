%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{revComp.NucleotideSequence}
\alias{revComp.NucleotideSequence}
\alias{NucleotideSequence.revComp}
\alias{revComp,NucleotideSequence-method}

 \title{Reverse complmenet a NucleotideSequence object}

 \description{
	Reverse complmenet a NucleotideSequence object.

	The method reverse complements the sequence "in place", no object cloning is performed.
 }

 \usage{
\method{revComp}{NucleotideSequence}(this, ...)
}

 \arguments{
 	\item{this}{A NucleotideSequence object}
 	\item{...}{Not used.}
 }

 \value{
 	Invisible TRUE.
 }

 \examples{
	s<-NucleotideSequence(string="ATGC")
	s
	revComp(s)
	s
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{NucleotideSequence}}.
 }
