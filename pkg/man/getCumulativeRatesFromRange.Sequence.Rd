%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getCumulativeRatesFromRange.Sequence}
\alias{getCumulativeRatesFromRange.Sequence}
\alias{Sequence.getCumulativeRatesFromRange}
\alias{getCumulativeRatesFromRange,Sequence-method}

 \title{Get the cumulative site rates for a collection of Site objects aggregated by a Sequence object}

 \description{
	Get the cumulative site rates for a collection of Site objects aggregated by a Sequence object.
 }

 \usage{
\method{getCumulativeRatesFromRange}{Sequence}(this, index, ...)
}

 \arguments{
 	\item{this}{A Sequence object.}
	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
 	\item{...}{Not used.}
 }

 \value{
 	A numeric vector.
 }

 \examples{
	# create a sequence with some processes attached
	s<-Sequence(
               string="ATGC",
               alphabets=list(NucleotideAlphabet()),
               processes=list(list(JC69()),list(JC69(),GTR()))
               )
	# get cumulative rates for positions 1 and 3
	getCumulativeRatesFromRange(s,c(1,3))
	# get all cumulative rates via virtual field
	s$cumulativeRates	# via the "getCumulativeRates.Sequence" method
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Sequence}}.
 }
