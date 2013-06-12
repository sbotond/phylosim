%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{as.character.Event}
\alias{as.character.Event}
\alias{Event.as.character}
\alias{as.character,Event-method}

 \title{Get the character representation of an Event object}

 \description{
	Get the character representation of an Event object.

	The character represenation of an Event object has the following format:
	"event name" ("event rate") <-- "generator process id", like \code{"A->T (0.333333333333333) <-- JC69:Anonymous:44780832"}.
 }

 \usage{
\method{as.character}{Event}(x, ...)
}

 \arguments{
 	\item{x}{An Event object.}
 	\item{...}{Not used.}
 }

 \value{
 	A character vector of length one.
 }

 \examples{
	# create a sequence and attach a process
	s<-NucleotideSequence(string="ATGC",processes=list(list(JC69())))
	# get the first active event from the first site
	e<-s$sites[[1]]$events[[1]]
	# get the character representation of e
	as.character(e)
	# or more simply
	e
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Event}}.
 }
