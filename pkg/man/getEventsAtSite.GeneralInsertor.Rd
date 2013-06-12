%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getEventsAtSite.GeneralInsertor}
\alias{getEventsAtSite.GeneralInsertor}
\alias{GeneralInsertor.getEventsAtSite}
\alias{getEventsAtSite,GeneralInsertor-method}

 \title{Generate insertion event object given the state of the target site}

 \description{
	Generate insertion event object given the state of the target site.

	This method generates a list with one insertion event. The rate of the
	event is calculated as the product of the general rate of the process
	and the "rate.multiplier" site-process specific parameter. An empty list is
	returned if the rate is zero or NA.
 }

 \usage{
\method{getEventsAtSite}{GeneralInsertor}(this, target.site, ...)
}

 \arguments{
 	\item{this}{A GeneralInsertor object.}
 	\item{target.site}{A Site object.}
 	\item{...}{Not used.}
 }

 \value{
 	A list of Event objects.
 }

 \examples{
	# create a sequence object
	s<-NucleotideSequence(string="AAAA")
	# create a GeneralInsertor process, provide template sequence.
	# propsed insert lengths:3, always accept.
	i<-GeneralInsertor(
		rate=0.5,
		template.seq=NucleotideSequence(string="GGG"),
		propose.by=function(process){3},
		accept.by=function(process,sequence,window){TRUE}
	)
	# attach process to site
	s$processes<-list(list(i));
	# set rate multiplier
	setRateMultipliers(s,i,2)
	# get the list of active events from site 2
	events<-getEventsAtSite(i,s$sites[[2]])
	events
	# set postition for event
	e<-events[[1]]
	e$.position<-2
	# print sequence
	s
	# perform event
	Perform(e)
	# check sequence again
	s
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	GeneralInsertor GeneralInDel Process Event
 }
