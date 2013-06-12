%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{stringLength}
\alias{stringLength.default}
\alias{stringLength}


 \title{Returns the string length of the character representation of an object}

 \description{
	Returns the string length of the character representation of an object.
	More useful as a static method.
 }

 \usage{
\method{stringLength}{default}(this, ...)
}

 \arguments{
 	\item{this}{An object.}
 	\item{...}{Not used.}
 }

 \value{
 	An integer vector of length one.
 }

 \examples{
	x<-"character representaion"
	# get the strign length of x
	stringLength(x)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PSRoot}}.
 }

