%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setRateMultipliers.Sequence}
\alias{setRateMultipliers.Sequence}
\alias{Sequence.setRateMultipliers}
\alias{setRateMultipliers,Sequence-method}

 \title{Set the values of the rate multiplier parameters for a given Process object and a collection of Site  object aggregated by a Sequence object}

 \description{
	Set the values of the rate multiplier parameters for a given Process object and a collection of Site  object aggregated by a Sequence object.
	This method just calls \code{setParameterAtSites(this=this,process=process,id="rate.multiplier",value=value,index=index)} See setParameterAtSites.Sequence for details.
 }

 \usage{
\method{setRateMultipliers}{Sequence}(this, process, value, index, ...)
}

 \arguments{
 	\item{this}{A Sequence object.}
 	\item{process}{A valid Process object.}
	\item{value}{A numeric vector containing the new values of the site-process specific parameter, recycled if shorter than the index vector.}
	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
 	\item{...}{Not used.}
 }

 \value{
 	The Sequence object (invisible).
 }


 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	setParameterAtSites.Sequence
 }
