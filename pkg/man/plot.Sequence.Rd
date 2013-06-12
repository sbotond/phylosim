%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{plot.Sequence}
\alias{plot.Sequence}
\alias{Sequence.plot}
\alias{plot,Sequence-method}

 \title{Plot the total site rates for a collection of Site objects aggregated by a Sequence object}

 \description{
	Plot the total site rates for a collection of Site objects aggregated by a Sequence object.
 }

 \usage{
\method{plot}{Sequence}(x, index=NA, ...)
}

 \arguments{
 	\item{x}{A Sequence object.}
	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
 	\item{...}{Not used.}
 }

 \value{
 	Invisible TRUE or FALSE.
 }

 \examples{
	# create a nucleotide sequence with a process attached
	s<-NucleotideSequence(string="ATGGCCA",processes=list(list(JC69())))
	# plot total rates in range 1:4
	plot(s,1:4)
	# plot all total rates
	plot(s)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Sequence}}.
 }
