%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{detachProcess.Sequence}
\alias{detachProcess.Sequence}
\alias{Sequence.detachProcess}
\alias{detachProcess,Sequence-method}

 \title{Detach a Process object from a set of Site objects aggregated by a Sequence object}

 \description{
	Detach a Process object from a set of Site objects aggregated by a Sequence object.
 }

 \usage{
\method{detachProcess}{Sequence}(this, process, index, ...)
}

 \arguments{
 	\item{this}{A Sequence object.}
 	\item{process}{A Process object.}
 	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
 	\item{...}{Not used.}
 }

 \value{
 	The Sequence object (invisible).
 }

 \examples{
	# create a sequence object with two processes attached
	p<-JC69()
	s<-Sequence(length=4,alphabets=list(NucleotideAlphabet()),processes=list(list(p,K80())))
	# get the list of attached processes
	s$processes
	# detach JC69 from range c(1,4)
	detachProcess(s,p,c(1,4))
	s$processes
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Sequence}}.
 }
