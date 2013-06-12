%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setHandler.Event}
\alias{setHandler.Event}
\alias{Event.setHandler}
\alias{setHandler,Event-method}

 \title{Forbidden action: setting the handler function of an Event object}

 \description{
       Forbidden action: setting the handler function of an Event object.
	The handler function is tipically set by a \code{getEventsAtSite.*} method generating the Event object
	by directly modifying the this$.handler field or by the \code{.setHandler()} method.
 }

 \usage{
\method{setHandler}{Event}(this, value, ...)
}

 \arguments{
       \item{this}{An object.}
       \item{value}{Not used.}
       \item{...}{Not used.}
 }

 \value{
	Throws an error.
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
       For more information see \code{\link{Event}}.
 }
