%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setRate.QMatrix}
\alias{setRate.QMatrix}
\alias{QMatrix.setRate}
\alias{setRate,QMatrix-method}

 \title{Set an unscaled rate in a QMatrix object}

 \description{
       Set an unscaled rate in a QMatrix object.
     
       This method sets the element corresponding to a given event in the unscaled rate matrix.
       The event can be specified by the inital and target states ("from" and "to" arguments), or by the
       event name ("from->to"). The event name takes precedence over the "from" and "to" arguments.

 }

 \usage{
\method{setRate}{QMatrix}(this, name=NA, value, from=NA, to=NA, scale=TRUE, diag=TRUE, guess.equ=TRUE, ...)
}

 \arguments{
       \item{this}{A QMatrix object.}
       \item{name}{The name of the event.}
       \item{value}{The new value of the rate.}
       \item{from}{The initial state.}
       \item{to}{Target state.}
       \item{scale}{Call rate rescaling.}
       \item{diag}{Calculate diagonal elements.}
       \item{guess.equ}{Guess equilibrium distribution.}
       \item{...}{Not used.}
 }

 \value{
       A Numeric vector of length one.
 }

 \examples{
       # create a QMatrix object
       # provide an Alphabet object and the rates
       m<-QMatrix(alphabet=BinaryAlphabet(), rate.list=list("1->0"=1,"0->1"=1))
       # set the unscaled rate by event name
       setRate(m,"0->1",2)
       # get the unscaled rate of "0->1" by name
       getRate(m,"0->1")     
       # set the unscaled rate by states
       setRate(m,"0->1",0.5)
       # set the unscaled rate of "0->1" by states
       setRate(m,"0->1",0.5)
       # get the unscaled rate of "0->1" by states
       getRate(m,from="0",to="1")
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
       For more information see \code{\link{QMatrix}}.
 }
