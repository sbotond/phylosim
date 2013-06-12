%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{checkConsistency.PSRoot}
\alias{checkConsistency.PSRoot}
\alias{PSRoot.checkConsistency}
\alias{checkConsistency,PSRoot-method}

 \title{Check object consistency}

 \description{
		Check object consistency.
		The consisntency check is not implemented in plain PSRoot objects,
		the method prints out a warning about that.
 }

 \usage{
\method{checkConsistency}{PSRoot}(this, ...)
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
 	For more information see \code{\link{PSRoot}}.
 }
