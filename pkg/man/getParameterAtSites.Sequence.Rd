%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getParameterAtSites.Sequence}
\alias{getParameterAtSites.Sequence}
\alias{Sequence.getParameterAtSites}
\alias{getParameterAtSites,Sequence-method}

 \title{Get the values of a site-process specific paramater for a process and a collection of Site objects aggregated by a Sequence object}

 \description{
	Get the values of a site-process specific paramater for a process and a collection of Site objects aggregated by a Sequence object.
 }

 \usage{
\method{getParameterAtSites}{Sequence}(this, process, id, index, ...)
}

 \arguments{
 	\item{this}{A Sequence object.}
 	\item{process}{A valid Process object.}
 	\item{id}{The identifier of the site-process specific parameter.}
	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
 	\item{...}{Not used.}
 }

 \value{
 	A list of site specific paramters. A site specific paramter is a list storing the id, the name, the value
	and the type of the parameter.
 }

 \examples{
	# create a sequence, attach a process
	p<-K80()
	s<-Sequence(length=6,alphabets=list(NucleotideAlphabet()),processes=list(list(p)))
	# set a new pattern of rate multipliers in the
   # range 1:3, the default value is 1.0 by the way
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
