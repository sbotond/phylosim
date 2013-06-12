%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{checkConsistency.Sequence}
\alias{checkConsistency.Sequence}
\alias{Sequence.checkConsistency}
\alias{checkConsistency,Sequence-method}

 \title{Check object consistency}

 \description{
		Check object consistency.
 }

 \usage{
\method{checkConsistency}{Sequence}(this, omit.sites=FALSE, ...)
}

 \arguments{
       \item{this}{An object.}
       \item{omit.sites}{Do not check aggregated site objects.}
       \item{...}{Not used.}
 }


 \value{
		Returns an invisible TRUE if no inconsistencies found in the object, throws
		an error otherwise.
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Sequence}}.
 }
