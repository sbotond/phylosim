%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getTotalRatesFromRange.Sequence}
\alias{getTotalRatesFromRange.Sequence}
\alias{Sequence.getTotalRatesFromRange}
\alias{getTotalRatesFromRange,Sequence-method}

 \title{Get the vector of total site rates for a collection of Site objects aggregated by a Sequence object}

 \description{
	Get the vector of total site rates for a collection of Site objects aggregated by a Sequence object.
 }

 \usage{
\method{getTotalRatesFromRange}{Sequence}(this, index, ...)
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
	# get total rates for positions 1 and 3
	getTotalRatesFromRange(s,c(1,3))
	# get all total rates via virtual field
	s$totalRates	# via the "getTotalRates.Sequence" method
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Sequence}}.
 }
