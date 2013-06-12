%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getType.Alphabet}
\alias{getType.Alphabet}
\alias{Alphabet.getType}
\alias{getType,Alphabet-method}

 \title{Get Alphabet obejct type}

 \description{
	Get Alphabet obejct type.
 }

 \usage{
\method{getType}{Alphabet}(this, ...)
}

 \arguments{
 	\item{this}{An Alphabet object.}
 	\item{...}{Not used.}
 }

 \value{
 	A character vector of length one.
 }

 \examples{

	# create alphabet object
	a<-Alphabet(symbols=c(0,1),type="Binary");
	# get alphabet type
	getType(a)
	a$type
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Alphabet}}.
 }
