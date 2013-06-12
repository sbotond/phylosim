%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{omegaVarM3.CodonSequence}
\alias{omegaVarM3.CodonSequence}
\alias{CodonSequence.omegaVarM3}
\alias{omegaVarM3,CodonSequence-method}

 \title{The M3 (discrete) model of variable omega ratios among sites}

 \description{
	The M3 (discrete) model of variable omega ratios among sites.

	This method sets the \code{omega} site-process specific parameter
	in the specified range to values sampled from the M3 (discrete) model of
	variable omega ratios among sites.

       Distribution of omega values:
       \preformatted{
	CATEGORY	PROBABILITY

	omega_0		p0
	omega_1		p1
	omega_2 	p2
	...		...
	omega_k		pk
       }
 }

 \references{
 Yang, Z., Nielsen, R., Goldman, N., Pedersen Krabbe, A-M. (2000) Codon-Substitution Models for Heterogeneous Selection Pressure at Amino Acid Sites - Genetics 155:431-449 \url{http://bit.ly/bvjucn}

 Goldman, N., Yang, Z. (1994) A codon-based model of nucleotide substitution for protein-coding DNA sequences - Mol Biol Evol 11(5):725-36 \url{http://bit.ly/aSVEoa}
 }

 \usage{
\method{omegaVarM3}{CodonSequence}(this, process, omegas, probs, index, ...)
}

 \arguments{
 	\item{this}{A CodonSequence object.}
 	\item{process}{A process object inheriting from GY94.}
	\item{omegas}{A vector of omega values (omega_0 ... omega_k).}
	\item{probs}{A vector of probabilities (p0 ... pk).}
	\item{index}{A vector of positions.}
 	\item{...}{Not used.}
 }

 \value{
 	Invisible TRUE.
 }

 \examples{
	# create a GY94 object
	p<-GY94(kappa=2)
	# create a CodonSequence object, attach process p
	s<-CodonSequence(length=25, processes=list(list(p)))
	# sample states
	sampleStates(s)
	# sample omegas in range 1:20 from model M3
	omegaVarM3(s,p,omegas=c(0,2,4),probs=c(1/3,1/3,1/3),1:20)
	# get omega values
	getOmegas(s,p)
	# get a histogram of omega values in range 1:20
	omegaHist(s,p,breaks=50,1:20)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{CodonSequence}}.
 }
