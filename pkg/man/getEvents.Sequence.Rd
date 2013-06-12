%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getEvents.Sequence}
\alias{getEvents.Sequence}
\alias{Sequence.getEvents}
\alias{getEvents,Sequence-method}

 \title{Get the list of active Event objects for a set of Site objects aggregated by a Sequence object}

 \description{
	Get the list of active Event objects for a set of Site objects aggregated by a Sequence object.
 }

 \usage{
\method{getEvents}{Sequence}(this, index, ...)
}

 \arguments{
 	\item{this}{A Sequence object.}
	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
 	\item{...}{Not used.}
 }

 \value{
 	A list of Event objects.
 }

 \examples{
	# create a sequence with a process attached
	s<-Sequence(
               string="ATGC",
               alphabets=list(NucleotideAlphabet()),
               processes=list(list(JC69()))
               )
	# get the active events from range 1:3
	getEvents(s,1:3)
	# get all active events via virtual field
	s$events
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Sequence}}.
 }
