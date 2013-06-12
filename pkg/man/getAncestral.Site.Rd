%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getAncestral.Site}
\alias{getAncestral.Site}
\alias{Site.getAncestral}
\alias{getAncestral,Site-method}

 \title{Get the ancestral object of a Site object}

 \description{
	Get the ancestral object of a Site object.
 }

 \usage{
\method{getAncestral}{Site}(this, ...)
}

 \arguments{
 	\item{this}{A Site object.}
 	\item{...}{Not used.}
 }

 \value{
 	A Site object, a Process object or NA.
 }

 \examples{
	a<-Alphabet();
	# create some site object
	s1<-Site(alphabet=a)
	s2<-Site(ancestral=s1, alphabet=a)
	# get ancestral objects
	getAncestral(s1)
	s2$ancestral
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Site}}.
 }
