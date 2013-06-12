%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{as.character.Process}
\alias{as.character.Process}
\alias{Process.as.character}
\alias{as.character,Process-method}

 \title{Get the character representation of a Process object}

 \description{
       Get the character representation of a Process object.
	The string returned is the unique Process object identifier (class name + process name + object hash).
 }

 \usage{
\method{as.character}{Process}(x, ...)
}

 \arguments{
       \item{x}{A Process object}
       \item{...}{Not used.}
 }

 \value{
  A character vector of length one.
 }

 \examples{

       # create a Process object
       p<-Process()
	# get charatcer representation
	x<-as.character(p)
	print(x)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
       For more information see \code{\link{Process}}.
 }
