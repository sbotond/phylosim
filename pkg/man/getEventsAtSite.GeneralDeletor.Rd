%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getEventsAtSite.GeneralDeletor}
\alias{getEventsAtSite.GeneralDeletor}
\alias{GeneralDeletor.getEventsAtSite}
\alias{getEventsAtSite,GeneralDeletor-method}

 \title{Title}

 \description{
	Title.

	This method generates a list containing a single deletion event object. The rate
	of the event is calculated as the product of the general rate of the process and the
	"rate.multiplier" site-process specific parameter. An empty list is
       returned if the rate is zero or NA.
 }

 \usage{
\method{getEventsAtSite}{GeneralDeletor}(this, target.site, ...)
}

 \arguments{
 	\item{this}{A GeneralDeletor object.}
	\item{target.site}{The target Site object.}
 	\item{...}{Not used.}
 }

 \value{
 	A list of event objects.
 }

 \examples{
	# create the Sequence object
	s<-NucleotideSequence(string="ATGCCCGGCGGATTTATTA");
	# create a GeneralDeletor object
	# proposed deletion length: 4, always accept
	d<-GeneralDeletor(
			name = "Del Bosque",
			rate = 0.5,
			propose.by=function(process, sequence, position){ 4 },
			accept.by=function(process, sequence, range){ TRUE }
 	)
	# attach process to site
	attachProcess(s,d);
	# set the rate multiplier
	setRateMultipliers(s,d,2)
	# get the list of active events at site 6
	events<-getEventsAtSite(d,s$sites[[6]])
	events;
	# print sequence
	s
	# set the position for the event object
	e<-events[[1]];
	e$.position<-6;
	# perform the deletion event
	Perform(e)
	# check the results
	s
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{GeneralDeletor}}.
 }
