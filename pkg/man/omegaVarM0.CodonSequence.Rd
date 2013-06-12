%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{omegaVarM0.CodonSequence}
\alias{omegaVarM0.CodonSequence}
\alias{CodonSequence.omegaVarM0}
\alias{omegaVarM0,CodonSequence-method}

 \title{The M0 (one-ratio) model of variable omega ratios among sites}

 \description{
	The M0 (one-ratio) model of variable omega ratios among sites.

	This method sets the \code{omega} site-process specific parameter
	in the specified range to values sampled from the M0 (one-ratio) model of
	variable omega ratios among sites.

       Distribution of omega values:
       \preformatted{
	CATEGORY	PROBABILITY

	omega		1
       }
 }

 \references{
 Yang, Z., Nielsen, R., Goldman, N., Pedersen Krabbe, A-M. (2000) Codon-Substitution Models for Heterogeneous Selection Pressure at Amino Acid Sites - Genetics 155:431-449 \url{http://bit.ly/bvjucn}

 Goldman, N., Yang, Z. (1994) A codon-based model of nucleotide substitution for protein-coding DNA sequences - Mol Biol Evol 11(5):725-36 \url{http://bit.ly/aSVEoa}
 }

 \usage{
\method{omegaVarM0}{CodonSequence}(this, process, omega, index, ...)
}

 \arguments{
 	\item{this}{A CodonSequence object.}
 	\item{process}{A process object inheriting from GY94.}
	\item{omega}{The fixed omega value.}
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
	s<-CodonSequence(length=20, processes=list(list(p)))
	# sample states
	sampleStates(s)
	# sample omegas in range 1:5 from model M0
	omegaVarM0(s,p,omega=2,1:5)
	# get omega values
	getOmegas(s,p)
	# get a histogram of omega values in range 1:5
	omegaHist(s,p,breaks=50,1:5)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{CodonSequence}}.
 }
