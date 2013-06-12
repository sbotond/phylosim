%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{Perform.Event}
\alias{Perform.Event}
\alias{Event.Perform}
\alias{Perform,Event-method}

 \title{Perform an event}

 \description{

	Performing an event means that the modifications described by the Event object are actually made by calling
	the event handler function as returned by \code{getHandler} with the Event object as the first argument.

	The event won't be performed if the handler function is invalid, if there is no associated Site object,
	if the site position is undefined, if the rate is undefined, or if the generator process is invalid.

	The handler function will be overwritten after performing an event, so the Perform method should be called
	only once for every Event object.
 }

 \usage{
\method{Perform}{Event}(this, ...)
}

 \arguments{
 	\item{this}{An Event object.}
 	\item{...}{Not used.}
 }

 \value{
 	The value returned by the handler function.
 }

 \examples{
	# create a sequence and attach a process
	s<-NucleotideSequence(string="ATGC",processes=list(list(JC69())))
	# get the first active event from the first site
   #only Sequence methods set .position,
   #so s$sites[[1]]$events[[1]] wouldn't work.
	e<-getEvents(s,1)[[1]]
	# perform e
	Perform(e)
	# check the effect of the event on s
	s
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Event}}.
 }
