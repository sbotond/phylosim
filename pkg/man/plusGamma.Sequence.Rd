%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{plusGamma.Sequence}
\alias{plusGamma.Sequence}
\alias{Sequence.plusGamma}
\alias{plusGamma,Sequence-method}

 \title{Sample the rate multiplier parameters of a Process from a Gamma distribution for a collection of Site objects aggregated by a Sequence object}

 \description{
	Sample the rate multiplier parameters of a Process from a Gamma distribution for a collection of Site objects aggregated by a Sequence object.

	The gamma distribution is discretized by calculating the means of the portions corresponding
	to the categories having equal probabilities. If the \code{ncat} argument is not numeric, the
	rates are sampled from the continuous gamma distribution.
 }

 \references{
	Yang, Z. (1994) Maximum likelihood phylogenetic estimation from DNA sequences with variable
	rates over sites: approximate methods - Journal of Molecular Evolution 39:306-314 \url{http://dx.doi.org/10.1007/BF00160154}
 }

 \usage{
\method{plusGamma}{Sequence}(this, process, shape, index, ncat=4, ...)
}

 \arguments{
 	\item{this}{A Sequence object.}
 	\item{process}{A Process object.}
 	\item{shape}{The shape parameter of the gamma distribution.}
 	\item{index}{A vector of positions.}
 	\item{ncat}{Numer of categories in the discretized gamma distribution (4 by default).}
 	\item{...}{Not used.}
 }

 \value{
 	The Sequence object (invisible);
 }

 \examples{
	# create a sequence
	s<-NucleotideSequence(length=20)
	# attach a process
	p<-JC69()
	attachProcess(s,p)
	# get rate multipliers
	getRateMultipliers(s,p)	# the default value is 1.0
	# sample rate multipliers in range 1:5 from a discrete
	#gamma distribution with shape parameter 0.5
	plusGamma(s,p,0.5,1:5)
	# get rate multipliers
	getRateMultipliers(s,p)	# the default value is 1.0
	# sample rates from a continuous gamma distribution
	# with shape parameter 0.5
	plusGamma(s,p,0.5,ncat="cont")
	# get rate multipliers
	getRateMultipliers(s,p)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Sequence}}.
 }
