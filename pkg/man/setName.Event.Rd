%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setName.Event}
\alias{setName.Event}
\alias{Event.setName}
\alias{setName,Event-method}

 \title{Set the name of an Event object}

 \description{
	Set the name of an Event object.
 }

 \usage{
\method{setName}{Event}(this, new.name, ...)
}

 \arguments{
 	\item{this}{An Event object.}
 	\item{new.name}{A character vector of length one.}
 	\item{...}{Not used.}
 }

 \value{
 	The new name (invisible).
 }

 \examples{
	# create an Event object
	e<-Event()
	# set event name
	setName(e,"Insertion")
	# get event name
	e$name
	# set name via virtual field
	e$name<-"Deletion"
	e$name
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Event}}.
 }
