%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getEventRate.QMatrix}
\alias{getEventRate.QMatrix}
\alias{QMatrix.getEventRate}
\alias{getEventRate,QMatrix-method}

 \title{Get the unscaled rate of an event from a QMatrix object}

 \description{
       Get the unscaled rate of an event from a QMatrix object.
     
       This method return the element corresponding to a given event from the scaled rate matrix stored in a QMatrix object.
       The event can be specified by the inital and target states ("from" and "to" arguments), or by the
       event name ("from->to"). The event name takes precedence over the "from" and "to" arguments.

	This method returns NA if the resacling of the rates was not performed.
	The scaling is performed by the \code{rescaleQMatrix.GeneralSubstitution} method.
 }

 \usage{
\method{getEventRate}{QMatrix}(this, name=NA, from=NA, to=NA, ...)
}

 \arguments{
       \item{this}{A QMatrix object.}
       \item{name}{The name of the event.}
       \item{from}{The initial state.}
       \item{to}{Target state.}
       \item{...}{Not used.}
 }

 \value{
       A Numeric vector of length one.
 }

 \examples{
       # create a QMatrix object
       # provide an Alphabet object and the rates
       m<-QMatrix(alphabet=BinaryAlphabet(), rate.list=list("0->1"=1,"1->0"=1))
       # get the unscaled rate of "0->1" by name
       getEventRate(m,"0->1")  # retruns NA
       # get the unscaled rate of "0->1" by states
       getEventRate(m,from="0",to="1")	# returns NA
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
       For more information see \code{\link{QMatrix}}.
 }
