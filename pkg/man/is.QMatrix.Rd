%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{is.QMatrix}
\alias{is.QMatrix.default}
\alias{is.QMatrix}

 \title{Check if an object is an instance of the QMatrix class}

 \description{
       Check if an object is an instance of the QMatrix class.
 }

 \usage{
\method{is.QMatrix}{default}(this, ...)
}

 \arguments{
       \item{this}{An object.}
       \item{...}{Not used.}
 }

 \value{
       TRUE or FALSE.
 }

 \examples{
       # create some objects
       m<-QMatrix()
       p<-Process()
       # chek if they inherit from QMatrix
       is.GeneralSubstitution(m)
       is.GeneralSubstitution(p)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
       For more information see \code{\link{QMatrix}}.
 }

