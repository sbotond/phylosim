%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{stringLengthVector}
\alias{stringLengthVector.default}
\alias{stringLengthVector}


 \title{Returns the string lengths of the character represenations of a collection of objects}

 \description{
	Returns the string lengths of the character represenations of a collection of objects.
	More useful as a static method.
 }

 \usage{
\method{stringLengthVector}{default}(this, ...)
}

 \arguments{
 	\item{this}{An array or a list of object.}
 	\item{...}{Not used.}
 }

 \value{
 	An integer vector with the corresponding lengths.
 }

 \examples{
	x<-c("character representaion","other string");
	# get the strign length of x
	stringLengthVector(x)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PSRoot}}.
 }

