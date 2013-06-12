%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{plotParametersAtSites.Sequence}
\alias{plotParametersAtSites.Sequence}
\alias{Sequence.plotParametersAtSites}
\alias{plotParametersAtSites,Sequence-method}

 \title{Plot the value of a site-process specifc paramter for a collection of Site objects aggregated by a Sequence object}

 \description{
	Plot the value of a site-process specifc paramter for a collection of Site objects aggregated by a Sequence object.
	The type of the paramter must be numeric. The Process object must be attached to all positions specified
	in the index vector.
 }

 \usage{
\method{plotParametersAtSites}{Sequence}(this, process, id, index, ...)
}

 \arguments{
 	\item{this}{A Sequence object.}
 	\item{process}{A Process object.}
 	\item{id}{The identifier of the site-process specific parameter.}
	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
 	\item{...}{Not used.}
 }

 \value{
 	Invisible TRUE or FALSE.
 }

 \examples{
	# create a nucleotide sequence with a process attached
	p<-JC69()
	s<-NucleotideSequence(string="ATGGCCA",processes=list(list(p)))
	# plot rate multipliers in range 1:4
	plotParametersAtSites(s,p,"rate.multiplier",1:4)
	# plot rate multiplier for the full sequence
	plotParametersAtSites(s,p,"rate.multiplier")
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	Site Process Sequence
 }
