%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setAlphabet.Site}
\alias{setAlphabet.Site}
\alias{Site.setAlphabet}
\alias{setAlphabet,Site-method}

 \title{Attach an Alphabet object to a Site object}

 \description{
	Attach an Alphabet object to a Site object.
	If the ancestral site is not NA, then the symbol set of the ancestral Alphabet object and the new Alphabet
	object must be the same. The current state must be in the symbol set of the new Alphabet object, unless it is NA.
 }

 \usage{
\method{setAlphabet}{Site}(this, new.alphabet, ...)
}

 \arguments{
 	\item{this}{A Site object.}
 	\item{new.alphabet}{A valid Alphabet object.}
 	\item{...}{Not used.}
 }

 \value{
 	Returns the new Alphabet object (invisible).
 }

 \examples{

	# create a site object
	s<-Site()
	# create an Alphabet object
	a<-Alphabet(c("A","T","G"))
	# attach alphabet to site object
	setAlphabet(s,a)
	# set site state
	s$state<-"A"
	# clone the alphabet object
	b<-clone(a)
	# modify symbol set in b
	b$symbols<-c(b$symbols,"C")
	# attach b to s via virtual field
	s$alphabet<-b
	s$alphabet
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Site}}.
 }
