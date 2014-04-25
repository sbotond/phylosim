%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setRate.ToleranceSubstitution}
\alias{setRate.ToleranceSubstitution}
\alias{ToleranceSubstitution.setRate}
\alias{setRate,ToleranceSubstitution-method}

 \title{Set an unscaled rate for an event from a ToleranceSubstitution object}

 \description{
	Set an unscaled rate for an event from a ToleranceSubstitution object.

	This method sets the element corresponding to a given event in the unscaled Q matrix.
	The event can be specified by the inital and target states ("from" and "to" arguments), or by the
	event name ("from->to"). The event name takes precedence over the "from" and "to" arguments.

	Modifying any rate in the unscaled Q matrix will trigger the re-scaling of the whole matrix.
	The rescaled rates (used during simulations) are returned by the \code{getEventRate} method.

	This method doesn't modify the site specific rate multipliers.
 }

 \usage{
\method{setRate}{ToleranceSubstitution}(this, name=NA, value, from=NA, to=NA, ...)
}

 \arguments{
 	\item{this}{A ToleranceSubstitution object.}
	\item{name}{The name of the event.}
	\item{from}{The initial state.}
	\item{value}{The new value of the rate.}
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
	# set the unscaled rate by event name
	setRate(p,"0->1",2)
	# get the unscaled rate of "0->1" by name
	getRate(p,"0->1")
	# set the unscaled rate by states
	setRate(p,"0->1",0.5)
	# get the unscaled rate of "0->1" by states
	getRate(p,from="0",to="1")
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{ToleranceSubstitution}}.
 }
