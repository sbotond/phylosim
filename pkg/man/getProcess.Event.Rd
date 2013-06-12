%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getProcess.Event}
\alias{getProcess.Event}
\alias{Event.getProcess}
\alias{getProcess,Event-method}

 \title{Get the Process object which generated an Event object}

 \description{
	Get the Process object which generated an Event object.
 }

 \usage{
\method{getProcess}{Event}(this, ...)
}

 \arguments{
 	\item{this}{An Event object.}
 	\item{...}{Not used.}
 }

 \value{
 	A Process object.
 }

 \examples{
	# create a sequence and attach a process
	s<-NucleotideSequence(string="ATGC",processes=list(list(JC69())))
	# get the first active event from the first site
	e<-s$sites[[1]]$events[[1]]
	# get the generator process for e
	e$process
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Event}}.
 }
