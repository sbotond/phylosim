%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getEventsAtSite.GeneralSubstitution}
\alias{getEventsAtSite.GeneralSubstitution}
\alias{GeneralSubstitution.getEventsAtSite}
\alias{getEventsAtSite,GeneralSubstitution-method}

 \title{Generate the list of active Event objects for a given attached Site object}

 \description{
	Generate the list of active Event objects for a given attached Site object.

	This is the single most important method in the \code{GeneralSubstitution} class. It generates a list of the active
	Event objects given the transition rate matrix (Q matrix) and the "rate.multiplier" Site-Process specific parameter.
	It returns an empty list if the state of the site is "NA".
 }

 \usage{
\method{getEventsAtSite}{GeneralSubstitution}(this, target.site, ...)
}

 \arguments{
 	\item{this}{A GeneralSubstitution object.}
 	\item{target.site}{A Site object. The GeneralSubstitution object must be attached to the Site object.}
 	\item{...}{Not used.}
 }

 \value{
 	A list of the active Event objects.
 }

 \examples{
	# create an Alphabet object
	a<-BinaryAlphabet()
	# create a Site object
	s<-Site(alphabet=a);
	# create a GeneralSubstitution object
	p<-GeneralSubstitution(alphabet=a,rate.list=list("0->1"=1,"1->0"=1))
	# attach process p to site object s
	attachProcess(s,p)
	# get the rate of active events
	getEventsAtSite(p,s)	# empty list
	# set the state of s
	s$state<-1;
	# get the rate of active events
	getEventsAtSite(p,s)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{GeneralSubstitution}}.
 }
