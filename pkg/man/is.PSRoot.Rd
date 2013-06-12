%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{is.PSRoot}
\alias{is.PSRoot.default}
\alias{is.PSRoot}

 \title{Check if an object inherits from PSRoot}

 \description{
	Check if an object inherits from PSRoot.
 }

 \usage{
\method{is.PSRoot}{default}(this, ...)
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
	o<-PSRoot()
	a<-Alphabet()
	x<-Object()
	# check if they inherit form PSRoot
	is.PSRoot(o)
	is.PSRoot(a)
	is.PSRoot(x)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PSRoot}}.
 }

