%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getEventsAtSite.Process}
\alias{getEventsAtSite.Process}
\alias{Process.getEventsAtSite}
\alias{getEventsAtSite,Process-method}

 \title{Generate the list of active Event objects given a Site object}

 \description{
	Generate the list of active Event objects given a Site object.
	The Process object must be attached to the specified Site object.

	This method is crucial for the simulations. For the Process class it returns an empty list.
	Descendant classes should implement meaningful getEventsAtSite methods.
 }

 \usage{
\method{getEventsAtSite}{Process}(this, site, position, ...)
}

 \arguments{
 	\item{this}{A Process object.}
 	\item{site}{A valid Site object.}
 	\item{position}{The position of the site in the enclosing Sequence object (if any).}
 	\item{...}{Not used.}
 }

 \value{
 	An empty list.
 }

 \examples{
	# create objects
	a<-NucleotideAlphabet()
	p<-Process(alphabet=a)
	s<-Site(alphabet=a)
	# attach Process p to Site s
	s$processes<-list(p)
	# get active Event objects (empty list)
	getEventsAtSite(p,s)

 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Process}}.
 }
