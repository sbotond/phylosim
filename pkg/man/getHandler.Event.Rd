%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getHandler.Event}
\alias{getHandler.Event}
\alias{Event.getHandler}
\alias{getHandler,Event-method}

 \title{Get the handler function of an Event object}

 \description{
	Get the handler function of an Event object.
 }

 \usage{
\method{getHandler}{Event}(this, ...)
}

 \arguments{
 	\item{this}{An Event object.}
 	\item{...}{Not used.}
 }

 \value{
 	A function object.
 }

 \examples{
	# create a sequence and attach a process
	s<-NucleotideSequence(string="ATGC",processes=list(list(JC69())))
	# get the first active event from the first site
   # only Sequence methods set .position,
   # so s$sites[[1]]$events[[1]] wouldn't work.
	e<-getEvents(s,1)[[1]]
	# het the handler of e
	getHandler(e)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Event}}.
 }
