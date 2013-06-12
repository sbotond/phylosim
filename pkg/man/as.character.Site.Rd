%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{as.character.Site}
\alias{as.character.Site}
\alias{Site.as.character}
\alias{as.character,Site-method}

 \title{Get the character representation of a Site object}

 \description{
	Get the character representation of a Site object.

 }

 \usage{
\method{as.character}{Site}(x, ...)
}

 \arguments{
 	\item{x}{A Site object.}
 	\item{...}{Not used.}
 }

 \value{
 	A character vector of length one containing the current state.
 }

 \examples{
	# create site object
	s<-Site(alphabet=NucleotideAlphabet(),state="A")
	# get character represenation
	x<-as.character(s)
	x

 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Site}}.
 }
