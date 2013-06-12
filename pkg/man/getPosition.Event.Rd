%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getPosition.Event}
\alias{getPosition.Event}
\alias{Event.getPosition}
\alias{getPosition,Event-method}

 \title{Get the position of the Site object associated to an Event object in the enclosing Sequence object}

 \description{
	Get the position of the Site object associated to an Event object in the enclosing Sequence object.
 }

 \usage{
\method{getPosition}{Event}(this, ...)
}

 \arguments{
 	\item{this}{An Event object.}
 	\item{...}{Not used.}
 }

 \value{
 	A numeric vector of length one.
 }

 \examples{
	# create a sequence and attach a process
	s<-NucleotideSequence(string="ATGC",processes=list(list(JC69())))
	# get the first active event from the first site
	e<-getEvents(s,1)[[1]]
	# get the position of the site associated with e
	getPosition(e)
	# get position via virtual field
	e$position
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Event}}.
 }
