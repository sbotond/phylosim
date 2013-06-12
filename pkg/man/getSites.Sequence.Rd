%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getSites.Sequence}
\alias{getSites.Sequence}
\alias{Sequence.getSites}
\alias{getSites,Sequence-method}

 \title{Get the list of the Site object aggregated in a Sequence object}

 \description{
	Get the list of the Site object aggregated in a Sequence object.
	Warning: there is no setSites method!
 }

 \usage{
\method{getSites}{Sequence}(this, ...)
}

 \arguments{
 	\item{this}{A Sequence object.}
 	\item{...}{Not used.}
 }

 \value{
 	A list of Site objects.
 }

 \examples{
	# create a sequence object
	s<-Sequence(alphabets=list(NucleotideAlphabet()),string="AATTGCCC")
	# get the list of aggregated Site objects
	getSites(s)
	# get Site objects via virtual field
	s$sites
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Sequence}}.
 }
