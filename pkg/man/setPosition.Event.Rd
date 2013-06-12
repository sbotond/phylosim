%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setPosition.Event}
\alias{setPosition.Event}
\alias{Event.setPosition}
\alias{setPosition,Event-method}

 \title{Set the position of the Site object associated to an Event object}

 \description{
	Set the position of the Site object associated to an Event object.

	The position field is usually not modified directly, but set by the \code{getEvents.Sequence} method.
	The position is *not* set by Site methods as \code{getEventsAtSite.Site}.
 }

 \usage{
\method{setPosition}{Event}(this, value, ...)
}

 \arguments{
 	\item{this}{An Event object.}
 	\item{value}{The position.}
 	\item{...}{Not used.}
 }

 \value{
 	The new position (invisible).
 }

 \examples{
	# Note: the following example is not too useful

	# create a sequence and attach a process
	s<-NucleotideSequence(string="ATGC",processes=list(list(JC69())))
	# get the first active event from the first site
	e<-getEvents(s,1)[[1]]
	# get event position
	e$position
	# set the position of the site associated with e
	setPosition(e,2)
	# get position via virtual field
	e$position
	# set position via virtual field
	e$position<-1
	e$position
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Event}}.
 }
