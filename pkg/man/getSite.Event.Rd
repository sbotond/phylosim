%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getSite.Event}
\alias{getSite.Event}
\alias{Event.getSite}
\alias{getSite,Event-method}

 \title{Get the Site object associated with an Event object}

 \description{
	Get the Site object associated with an Event object.
 }

 \usage{
\method{getSite}{Event}(this, ...)
}

 \arguments{
 	\item{this}{An Event object.}
 	\item{...}{Not used.}
 }

 \value{
 	A Site object.
 }

 \examples{
	# create a sequence and attach a process
	s<-NucleotideSequence(string="ATGC",processes=list(list(JC69())))
	# get the first active event from the first site
	e<-s$sites[[1]]$events[[1]]
	# get the site associated with e
	getSite(e)
	# get site via virtual field
	e$site
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Event}}.
 }
