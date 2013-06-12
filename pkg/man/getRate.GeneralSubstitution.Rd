%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getRate.GeneralSubstitution}
\alias{getRate.GeneralSubstitution}
\alias{GeneralSubstitution.getRate}
\alias{getRate,GeneralSubstitution-method}

 \title{Get an unscaled rate of an event from a GeneralSubstitution object}

 \description{
	Get an unscaled rate of an event from a GeneralSubstitution object.

	This method gets the element corresponding to a given event form the unscaled Q matrix.
	a given event. The event can be specified by the inital and target states ("from" and "to" arguments), or by the
	event name ("from->to"). The event name takes precedence over the "from" and "to" arguments.

	The rescaled rates (used during simulations) are returned by the \code{getEventRate} method.

	This method doesn't take into account the site specific rate multipliers in any way.
 }

 \usage{
\method{getRate}{GeneralSubstitution}(this, name=NA, from=NA, to=NA, ...)
}

 \arguments{
 	\item{this}{A GeneralSubstitution object.}
	\item{name}{The name of the event.}
	\item{from}{The initial state.}
	\item{to}{Target state.}
 	\item{...}{Not used.}
 }

 \value{
 	A Numeric vector of length one.
 }

 \examples{
	# create a GeneralSubstitution object
	# provide an Alphabet object and the rates
	p<-GeneralSubstitution(alphabet=BinaryAlphabet(), rate.list=list("1->0"=1,"0->1"=1))
	# get the unscaled rate of "0->1" by name
	getRate(p,"0->1")
	# get the unscaled rate of "0->1" by states
	getRate(p,from="0",to="1")
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{GeneralSubstitution}}.
 }
