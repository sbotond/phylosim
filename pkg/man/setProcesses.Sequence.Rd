%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setProcesses.Sequence}
\alias{setProcesses.Sequence}
\alias{Sequence.setProcesses}
\alias{setProcesses,Sequence-method}

 \title{Specify a set of Process objects to be attached to a set of Site objects aggregated by a Sequence object}

 \description{
	Specify a set of Process objects to be attached to a set of Site objects aggregated by a Sequence object.
	The Process objects in a given inner list correspond to the set of processes to be attached to one Site object.
	Process objects already attached to a given Site are skipped. Attached processes which are not memebers of the list
	are detached, so specifying an empty list will detach all processes.
 }

 \usage{
\method{setProcesses}{Sequence}(this, value, index, ...)
}

 \arguments{
 	\item{this}{A Sequence object.}
 	\item{value}{A list of list of Process objects, recycled if shorter than the index vector.}
	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
 	\item{...}{Not used.}
 }

 \value{
 	The Sequence object (invisible).
 }

 \examples{
	# create a sequence of length 5
	s<-Sequence(length=5,alphabets=list(NucleotideAlphabet()));
	# set a pattern of processes
	setProcesses(s,list(list(JC69(),K81())))
	# get attached processes
	s$processes
	# detach all processes from range 1:3
	setProcesses(s,list(list()),1:3)
	s$processes
	# detach all processes via virtual field
	s$processes<-list(list())
	# create a process pattern in the full sequence via virtual field
	s$processes<-list(list(JC69()),list(GTR(),K80()))
	s$processes
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Sequence}}.
 }
