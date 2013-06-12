%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setParameterAtSites.Sequence}
\alias{setParameterAtSites.Sequence}
\alias{Sequence.setParameterAtSites}
\alias{setParameterAtSites,Sequence-method}

 \title{Set the values of a site-process specific paramater for a process and a collection of Site objects aggregated by a Sequence object}

 \description{
	Set the values of a site-process specific paramater for a process and a collection of Site objects aggregated by a Sequence object.
 }

 \usage{
\method{setParameterAtSites}{Sequence}(this, process, id, value, index, ...)
}

 \arguments{
 	\item{this}{A Sequence object.}
 	\item{process}{A valid Process object.}
 	\item{id}{The identifier of the site-process specific parameter.}
	\item{value}{A vector containing the new values of the site-process specific parameter, recycled if shorter than the index vector. It should be consistent with the type of the parameter.}
	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
 	\item{...}{Not used.}
 }

 \value{
 	The Sequence object (invisible).
 }

 \examples{
	# create a sequence, attach a process
	p<-K80()
	s<-Sequence(length=6,alphabets=list(NucleotideAlphabet()),processes=list(list(p)))
	# set a new pattern of rate multipliers in the range 1:3,
   # the default value is 1.0 by the way
	setParameterAtSites(s,p,"rate.multiplier",c(2,3),1:3)
	# get rate multipliers
	getParameterAtSites(s,p,"rate.multiplier")
	# set a new value for the whole sequence
	setParameterAtSites(s,p,"rate.multiplier",0.5)
	# get rate multipliers
	getParameterAtSites(s,p,"rate.multiplier")
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	Site Process For more information see \code{\link{Sequence}}.
 }
