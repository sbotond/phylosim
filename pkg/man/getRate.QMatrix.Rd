%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getRate.QMatrix}
\alias{getRate.QMatrix}
\alias{QMatrix.getRate}
\alias{getRate,QMatrix-method}

 \title{Get an unscaled rate of an event from a QMatrix object}

 \description{
       Get an unscaled rate of an event from a QMatrix object.
     
       This method gets the element corresponding to a given event form the \emph{unscaled} rate matrix.
       a given event. The event can be specified by the inital and target states ("from" and "to" arguments), or by the
       event name ("from->to"). The event name takes precedence over the "from" and "to" arguments.

 }

 \usage{
\method{getRate}{QMatrix}(this, name=NA, from=NA, to=NA, ...)
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
       m<-QMatrix(alphabet=BinaryAlphabet(), rate.list=list("1->0"=1,"0->1"=1))
       # get the unscaled rate of "0->1" by name
       getRate(m,"0->1")     
       # get the unscaled rate of "0->1" by states
       getRate(m,from="0",to="1")
 }


 \seealso{
       For more information see \code{\link{QMatrix}}.
 }
