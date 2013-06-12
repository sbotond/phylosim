%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getRateMultipliers.Sequence}
\alias{getRateMultipliers.Sequence}
\alias{Sequence.getRateMultipliers}
\alias{getRateMultipliers,Sequence-method}

 \title{Get the values of the rate multiplier parameters for a given Process object and a collection of Site  object aggregated by a Sequence object}

 \description{
	Get the values of the rate multiplier parameters for a given Process object and a collection of Site  object aggregated by a Sequence object.
	This method just calls \code{getParameterAtSites(this=this,process=process,id="rate.multiplier",index=index)} See getParameterAtSites.Sequence for details.
 }

 \usage{
\method{getRateMultipliers}{Sequence}(this, process, index, ...)
}

 \arguments{
 	\item{this}{A Sequence object.}
 	\item{process}{A valid Process object.}
	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
 	\item{...}{Not used.}
 }

 \value{
 	A numeric vector with the current values of the rate multiplier in the specified range.
 }


 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	setParameterAtSites.Sequence
 }
