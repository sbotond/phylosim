%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{omegaVarM2.CodonSequence}
\alias{omegaVarM2.CodonSequence}
\alias{CodonSequence.omegaVarM2}
\alias{omegaVarM2,CodonSequence-method}

 \title{The M2 (selection) model of variable omega ratios among sites}

 \description{
	The M2 (selection) model of variable omega ratios among sites.

	This method sets the \code{omega} site-process specific parameter
	in the specified range to values sampled from the M2 (selection) model of
	variable omega ratios among sites.

       Distribution of omega values:
       \preformatted{
	CATEGORY	PROBABILITY

	omega_0 = 0	p0
	omega_1 = 1	p1
	omega_2 	1-p0-p1
       }
 }

 \references{
 Yang, Z., Nielsen, R., Goldman, N., Pedersen Krabbe, A-M. (2000) Codon-Substitution Models for Heterogeneous Selection Pressure at Amino Acid Sites - Genetics 155:431-449 \url{http://bit.ly/bvjucn}

 Goldman, N., Yang, Z. (1994) A codon-based model of nucleotide substitution for protein-coding DNA sequences - Mol Biol Evol 11(5):725-36 \url{http://bit.ly/aSVEoa}
 }

 \usage{
\method{omegaVarM2}{CodonSequence}(this, process, p0, p1, omega_2, index, ...)
}

 \arguments{
 	\item{this}{A CodonSequence object.}
 	\item{process}{A process object inheriting from GY94.}
	\item{p0}{See above.}
	\item{p1}{See above.}
	\item{omega_2}{See above.}
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
	# sample omegas in range 1:20 from model M2
	omegaVarM2(s,p,p0=0.2,p1=0.3,omega_2=4,1:20)
	# get omega values
	getOmegas(s,p)
	# get a histogram of omega values in range 1:20
	omegaHist(s,p,breaks=50,1:20)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{CodonSequence}}.
 }
