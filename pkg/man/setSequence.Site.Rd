%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setSequence.Site}
\alias{setSequence.Site}
\alias{Site.setSequence}
\alias{setSequence,Site-method}

 \title{Assotiate a Sequence object with a Site object}

 \description{
	Assotiate a Sequence object with a Site object.
 }

 \usage{
\method{setSequence}{Site}(this, new.seq, ...)
}

 \arguments{
 	\item{this}{A Site object.}
 	\item{new.seq}{A valid Sequence object.}
 	\item{...}{Not used.}
 }

 \value{
 	The Sequence object (invisible).
 }

 \examples{
	# create a site object
	s<-Site()
	# get associated Sequence object
	s$sequence
	# set associated Sequence object
	setSequence(s,Sequence())
	s$sequence

 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Site}}.
 }
