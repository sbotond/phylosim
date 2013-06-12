%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getName.Event}
\alias{getName.Event}
\alias{Event.getName}
\alias{getName,Event-method}

 \title{Get the name of an Event object}

 \description{
	Get the name of an Event object.
 }

 \usage{
\method{getName}{Event}(this, ...)
}

 \arguments{
 	\item{this}{An event object.}
 	\item{...}{Not used.}
 }

 \value{
 	A character vector of length one.
 }

 \examples{
	# create an Event object
	e<-Event(name="MyEvent")
	# get event name
	getName(e)
	# get name via virtual field
	e$name
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Event}}.
 }
