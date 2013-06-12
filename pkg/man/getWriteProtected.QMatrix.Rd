%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getWriteProtected.QMatrix}
\alias{getWriteProtected.QMatrix}
\alias{QMatrix.getWriteProtected}
\alias{getWriteProtected,QMatrix-method}

 \title{Check if the object is write protected}

 \description{
       Check if the object is write protected.

	QMatrix object do not have a write protection flag of their own, but they use the one from the
	associated Process object.
       Write protected objects cannot be modified through get/set methods and virtual fields.
 }

 \usage{
\method{getWriteProtected}{QMatrix}(this, ...)
}

 \arguments{
       \item{this}{A QMatrix object.}
       \item{...}{Not used.}
 }

 \value{
       TRUE or FALSE
 }


 \author{Botond Sipos, Gregory Jordan}

 \seealso{
       getWriteProtected.Process
 }
