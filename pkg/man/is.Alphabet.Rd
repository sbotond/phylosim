%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{is.Alphabet}
\alias{is.Alphabet.default}
\alias{is.Alphabet}

 \title{Check if an object is an instance of the Alphabet class}

 \description{
	Check if an object is an instance of the Alphabet class.
 }

 \usage{
\method{is.Alphabet}{default}(this, ...)
}

 \arguments{
 	\item{this}{An object.}
 	\item{...}{Not used.}
 }

 \value{
 	TRUE or FALSE
 }

 \examples{

	# create an alphabet object
	a<-Alphabet()
	# create a PSRoot object
	o<-PSRoot()
	# check if they are alphabet objects
	is.Alphabet(a)
	is.Alphabet(o)

 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Alphabet}}.
 }

