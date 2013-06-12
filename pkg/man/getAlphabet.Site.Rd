%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getAlphabet.Site}
\alias{getAlphabet.Site}
\alias{Site.getAlphabet}
\alias{getAlphabet,Site-method}

 \title{Get the Alphabet object attached to a Site object}

 \description{
	Get the Alphabet object attached to a Site object.

 }

 \usage{
\method{getAlphabet}{Site}(this, ...)
}

 \arguments{
 	\item{this}{A Site object.}
 	\item{...}{Not used.}
 }

 \value{
 	An Alphabet object.
 }

 \examples{

	# create a site object
	s<-Site()
	# create an Alphabet object
	a<-Alphabet(c("A","T","G"))
	# attach alphabet to site object
	setAlphabet(s,a)
	# get attached alphabet
	getAlphabet(s)
	# get attached alphabet via virtual field
	s$alphabet
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Site}}.
 }
