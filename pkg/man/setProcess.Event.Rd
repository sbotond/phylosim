%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setProcess.Event}
\alias{setProcess.Event}
\alias{Event.setProcess}
\alias{setProcess,Event-method}

 \title{Set the generator process for an Event object}

 \description{
	Set the generator process for an Event object.
 }

 \usage{
\method{setProcess}{Event}(this, new.proc, ...)
}

 \arguments{
 	\item{this}{An Event object.}
 	\item{new.proc}{A valid Process object.}
 	\item{...}{Not used.}
 }

 \value{
 	A Process object.
 }

 \examples{
	# create an Event object
	e<-Event()
	# set a generator process for e
	setProcess(e,Process())
	# get generator process
	e$process
	# set process via virtual field
	e$process<-K80()
	e$process
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Event}}.
 }
