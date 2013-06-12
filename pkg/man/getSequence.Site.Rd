%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getSequence.Site}
\alias{getSequence.Site}
\alias{Site.getSequence}
\alias{getSequence,Site-method}

 \title{Get the Sequence object associated with a given Site object}

 \description{
	Get the Sequence object associated with a given Site object.
 }

 \usage{
\method{getSequence}{Site}(this, ...)
}

 \arguments{
 	\item{this}{A Site object.}
 	\item{...}{Not used.}
 }

 \value{
 	A Sequence object or NA.
 }

 \examples{
	# create a site object
	s<-Site(sequence=Sequence())
	# get the associated Sequence object
	getSequence(s)
	# get the associated Sequence object via virtual field
	s$sequence

 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Site}}.
 }
