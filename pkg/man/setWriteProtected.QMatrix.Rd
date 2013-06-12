%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setWriteProtected.QMatrix}
\alias{setWriteProtected.QMatrix}
\alias{QMatrix.setWriteProtected}
\alias{setWriteProtected,QMatrix-method}

 \title{Set the write protection field for a QMatrix object}

 \description{
       Set the write protection field for a QMatrix object.

	QMatrix object do not have a write protection flag of their own, but they use the one from the
	associated Process object.
       Write protected objects cannot be modified through get/set methods and virtual fields.
 }

 \usage{
\method{setWriteProtected}{QMatrix}(this, value, ...)
}

 \arguments{
       \item{this}{A QMatrix object.}
	\item{value}{A logical vector of size one.}
       \item{...}{Not used.}
 }

 \value{
       TRUE or FALSE
 }


 \author{Botond Sipos, Gregory Jordan}

 \seealso{
       setWriteProtected.Process
 }
