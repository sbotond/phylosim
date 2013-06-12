%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setProcesses.Site}
\alias{setProcesses.Site}
\alias{Site.setProcesses}
\alias{setProcesses,Site-method}

 \title{Specify the list of Process objects attached to a Site object}

 \description{
	Specify the list of Process objects attached to a Site object.
	The Process objects in the "value" list correspond to the set of processes to be attached to the Site object.
       Process objects already attached to a given Site are skipped. Attached processes which are not memebers of the list
       are detached, so specifying an empty list will detach all processes.

	This method is an alternative to \code{attachProcess.Site} and \code{detachProcess.Site}, working with
	more than one process object.
 }

 \usage{
\method{setProcesses}{Site}(this, value, ...)
}

 \arguments{
 	\item{this}{A Site object.}
	\item{value}{A list of valid Process objects.}
 	\item{...}{Not used.}
 }

 \value{
 	The Site object (invisible).
 }

 \examples{
	# create a Site object
	s<-Site(alphabet=NucleotideAlphabet())
	# create some processes
	p1<-JC69(); p2<-K80(); p3<-DiscreteInsertor(rate=1);
	# attach the processes
	setProcesses(s,list(p1,p2,p3))
	# attach one more process via virtual field
	s$processes<-c(s$processes,list(GTR()))
	# get the list of attached processes
	s$processes
	# detach all processes via virtual field
	s$processes<-list()
	s$processes
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Site}}.
 }
