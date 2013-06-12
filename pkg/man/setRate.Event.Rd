%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setRate.Event}
\alias{setRate.Event}
\alias{Event.setRate}
\alias{setRate,Event-method}

 \title{Set the rate of an Event object}

 \description{
	Set the rate of an Event object.
 }

 \usage{
\method{setRate}{Event}(this, value, ...)
}

 \arguments{
 	\item{this}{An Event object.}
 	\item{value}{The event rate.}
 	\item{...}{Not used.}
 }

 \value{
 	The new value of the rate (invisible).
 }

 \examples{
	# create an Event object
	e<-Event(rate=0.1)
	# set  a new rate
	setRate(e,0.2)
	# get rate via virtual field
	e$rate
	# set rate via virtual field
	e$rate<-0.5
	e$rate
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Event}}.
 }
