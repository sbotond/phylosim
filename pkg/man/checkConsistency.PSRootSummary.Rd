%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{checkConsistency.PSRootSummary}
\alias{checkConsistency.PSRootSummary}
\alias{PSRootSummary.checkConsistency}
\alias{checkConsistency,PSRootSummary-method}

 \title{Check object consistency}

 \description{
		Check object consistency.
		The consisntency check is not implemented for PSRootSummary objects,
		the method prints out a warning about that.
 }

 \usage{
\method{checkConsistency}{PSRootSummary}(this, ...)
}

 \arguments{
       \item{this}{An object.}
       \item{...}{Not used.}
 }


 \value{
		Returns an invisible TRUE if no inconsistencies found in the object, throws
		an error otherwise.
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PSRootSummary}}.
 }
