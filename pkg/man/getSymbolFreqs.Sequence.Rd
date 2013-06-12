%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getSymbolFreqs.Sequence}
\alias{getSymbolFreqs.Sequence}
\alias{Sequence.getSymbolFreqs}
\alias{getSymbolFreqs,Sequence-method}

 \title{Get a table with the frequencies of the states of a collection of Site objects aggregated by a Sequence object}

 \description{
	Get a table with the frequencies of the states of a collection of Site objects aggregated by a Sequence object.
 }

 \usage{
\method{getSymbolFreqs}{Sequence}(this, index, ...)
}

 \arguments{
 	\item{this}{A Sequence object.}
	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
 	\item{...}{Not used.}
 }

 \value{
 	A table.
 }

 \examples{
	# create a nucleotide sequence
	s<-NucleotideSequence(length=30,processes=list(list(JC69())))
	# sample states
	sampleStates(s)
	# get state frequencies from ranges 1:10 and 20:30
	getSymbolFreqs(s,c(1:10,20:30))
	# get symbol frequencies for the full sequence
	getSymbolFreqs(s)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Sequence}}.
 }
