%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getCumulativeRates.Sequence}
\alias{getCumulativeRates.Sequence}
\alias{Sequence.getCumulativeRates}
\alias{getCumulativeRates,Sequence-method}

 \title{Get the total site rates from a Sequence object}

 \description{
	Get the total site rates from a Sequence object.
	This method simply calls \code{getCumulativeRatesFromRange(this)}.
	See \code{getCumulativeRates.Sequence} for more details.
 }

 \usage{
\method{getCumulativeRates}{Sequence}(this, ...)
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
 	getCumulativeRatesFromRange.Sequence
 }
