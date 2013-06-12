%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getEvents.Site}
\alias{getEvents.Site}
\alias{Site.getEvents}
\alias{getEvents,Site-method}

 \title{Get the list of active event objects given the current state of the Site object}

 \description{
	Get the list of active event objects given the current state of the Site object.
	The list of active event object might change according to the state of the Site object.
 }

 \usage{
\method{getEvents}{Site}(this, ...)
}

 \arguments{
 	\item{this}{A Site object.}
 	\item{...}{Not used.}
 }

 \value{
 	A list of event objects.
 }

 \examples{
	# create a site object with a JC69 substitution process attached
	s<-Site(alphabet=NucleotideAlphabet(), state="A",processes=list(JC69()))
	# get the list of active event objects
	getEvents(s)
	# modify site state
	s$state<-"T"
	# get the list of active event objects via virtual field
	s$events

 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Site}}.
 }
