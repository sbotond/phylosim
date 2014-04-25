%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getEventRateAtSite.ToleranceSubstitution}
\alias{getEventRateAtSite.ToleranceSubstitution}
\alias{ToleranceSubstitution.getEventRateAtSite}
\alias{getEventRateAtSite,ToleranceSubstitution-method}

 \title{Get the site spcific rate of an event from a ToleranceSubstitution object}

 \description{
	Get the site spcific rate of an event from a ToleranceSubstitution object.

	This method return the element from the associated QMatrix object corresponding to
	a given event multiplied by the "rate.multiplier" site-process specific parameter stored in the specified site object.
	The event can be specified by the inital and target states ("from" and "to" arguments), or by the
	event name ("from->to"). The event name takes precedence over the "from" and "to" arguments.

 }

 \usage{
\method{getEventRateAtSite}{ToleranceSubstitution}(this, site, name=NA, from=NA, to=NA, ...)
}

 \arguments{
 	\item{this}{A ToleranceSubstitution object. It must be attached to the provided Site object.}
 	\item{site}{A Site object.}
	\item{name}{The name of the event.}
	\item{from}{The initial state.}
	\item{to}{Target state.}
 	\item{...}{Not used.}
 }

 \value{
 	A Numeric vector of length one.
 }

 \examples{
	# create a ToleranceSubstitution object
	# provide an Alphabet object and the rates
	p<-ToleranceSubstitution(alphabet=BinaryAlphabet(), rate.list=list("1->0"=1,"0->1"=1))
	# create a Site object
	s<-Site(alphabet=BinaryAlphabet())
	# attach process p to site s
	s$processes<-list(p)
	# set the rate multiplier for s and p
       setParameterAtSite(p,s,id="rate.multiplier",value=2)
	# get the site specific rate of "0->1" by name
	getEventRateAtSite(p,s,"0->1")
	# get the site specific rate of "0->1" by states
	getEventRateAtSite(p,s,from="0",to="1")
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{ToleranceSubstitution}}.
 }
