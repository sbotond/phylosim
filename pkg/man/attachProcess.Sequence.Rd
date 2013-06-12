%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{attachProcess.Sequence}
\alias{attachProcess.Sequence}
\alias{Sequence.attachProcess}
\alias{attachProcess,Sequence-method}

 \title{Attach a Process object to a set of Site objects aggregated by a Sequence object}

 \description{
	Attach a Process object to a set of Site objects aggregated by a Sequence object.
 }

 \usage{
\method{attachProcess}{Sequence}(this, process, index, ...)
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
	# create a Sequence object of length 6
	s<-Sequence(length=10,alphabets=list(NucleotideAlphabet()))
	# attach a JC69 substitution process
	attachProcess(s,JC69())
	# get the list of attached processes
	s$processes
	# attach the GTR substitution process to range 3:6
	attachProcess(s,GTR(),3:6)
	# get the list of attached processes
	s$processes
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Sequence}}.
 }
