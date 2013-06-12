%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getTotalRates.Sequence}
\alias{getTotalRates.Sequence}
\alias{Sequence.getTotalRates}
\alias{getTotalRates,Sequence-method}

 \title{Get the total site rates from a Sequence object}

 \description{
	Get the total site rates from a Sequence object.
	This method simply calls \code{getTotalRatesFromRange(this)}.
	See \code{getTotalRatesFromRange.Sequence} for more details.
 }

 \usage{
\method{getTotalRates}{Sequence}(this, ...)
}

 \arguments{
 	\item{this}{A Sequence object.}
 	\item{...}{Not used.}
 }

 \value{
 	A numeric vector containing the total site rates.
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	getTotalRatesFromRange.Sequence
 }
