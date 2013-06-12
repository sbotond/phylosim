%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{GY94}
\docType{class}
\alias{GY94}


 \title{The GY94 class}

 \description{
	This class implements the codon substitution model of Goldman and Yang (1994).
	The transition/transversion rate ratio is stored in the \code{kappa} virtual field.
 	The nonsynonymous/synonymous substitution rate ratio (\code{omega}) is a site-process specific parameter
	with a default value of one.
	Hence, after the attachment of the process the variation of omega ratios among sites follows
	the M0 model (see Yang et al. 2000).

       The rate matrix of the \code{\link{GY94}} model is scaled in a way that the expected number
       of potential substiutions per site is equal to one at equlibrium.
       The \emph{codeml} program from the PAML package scales the rate matrix in order to have
       the expected number of accepted substiutions per site equal to one. Use the
	\code{\link{getOmegaScalingFactor.GY94}} method to claculate a branch length scaling factor
	which allows to switch to a PAML-style scaling given an average omega.

	If the \code{scale.nuc} constructor argument is TRUE, the rates of the returned \code{Event} objects
	will be multiplied by \code{3} to obtain a process which has the expected number of nucleotide substitutions
	(not \code{codon} substitutions) equal to one at equilibrium. This is useful when simulating
	mixed sequences. This option doesn't affect the rate matrix in any way.

	The M1-M4 models are implemented in the \code{omegaVarM[1-4].CodonSeqeunce} methods.
	Simulations under more complex models (M5-M13) can be achieved by first discretizing them
	using the \code{M5-13} tool from the INDELible software
	package (\url{http://abacus.gene.ucl.ac.uk/software/indelible/}).
	After discretization, the M5-M13 models can be simulated through the M3 (discrete) model.

	Package:   \cr
\bold{Class GY94}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link{PSRoot}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{\link{Process}}\cr
\code{~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~+--}\code{\link{GeneralSubstitution}}\cr
\code{~~~~~~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~~~~~~+--}\code{\link{CodonUNREST}}\cr
\code{~~~~~~~~~~~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~~~~~~~~~~~+--}\code{GY94}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{GY94}\cr
extends \link{CodonUNREST}\cr


 }

 \references{
 Goldman, N., Yang, Z. (1994) A codon-based model of nucleotide substitution for protein-coding DNA sequences - Mol Biol Evol 11(5):725-36 \url{http://bit.ly/aSVEoa}

 Yang, Z., Nielsen, R., Goldman, N., Pedersen Krabbe, A-M. (2000) Codon-Substitution Models for Heterogeneous Selection Pressure at Amino Acid Sites - Genetics 155:431-449 \url{http://bit.ly/bvjucn}
 }

 \usage{
GY94(
        name="Anonymous", table.id=1, kappa=1, 
        omega.default=1, codon.freqs=NA, scale.nuc=FALSE, ...
       )
}

 \arguments{
 	\item{name}{Name of the object.}
	\item{table.id}{The identifier of the genetic code table to use (1 by default).}
	\item{kappa}{The transition/transversion rate ratio (1 by default).}
	\item{omega.default}{The default value of the omega site-process specific parameter (1 by default).}
	\item{codon.freqs}{A vector of codon frequencies.}
	\item{scale.nuc}{Scale to nucleotide substitutions if TRUE (see above).}
 	\item{...}{Additional arguments.}
	}

 \section{Fields and Methods}{
 	\bold{Methods:}\cr
\tabular{rll}{
 \tab \code{checkConsistency} \tab  -\cr
 \tab \code{getCodonFreqs} \tab  -\cr
 \tab \code{getEventsAtSite} \tab  -\cr
 \tab \code{getKappa} \tab  -\cr
 \tab \code{getOmegaScalingFactor} \tab  -\cr
 \tab \code{is} \tab  -\cr
 \tab \code{setCodonFreqs} \tab  -\cr
 \tab \code{setKappa} \tab  -\cr
 \tab \code{setRate} \tab  -\cr
 \tab \code{summary} \tab  -\cr
}


 \bold{Methods inherited from CodonUNREST}:\cr
checkConsistency, is

 \bold{Methods inherited from GeneralSubstitution}:\cr
as.character, checkConsistency, clone, getAlphabet, getEquDist, getEventRate, getEventRateAtSite, getEventsAtSite, getQMatrix, getRate, getRateList, hasUndefinedRate, is, plot, rescaleQMatrix, sampleState, setAlphabet, setEquDist, setQMatrix, setRate, setRateList, summary

 \bold{Methods inherited from Process}:\cr
!=, ==, as.character, checkConsistency, clone, getAlphabet, getEventsAtSite, getId, getName, getParameterAtSite, getSiteSpecificParamIds, getSiteSpecificParamList, getWriteProtected, hasSiteSpecificParameter, hasUndefinedRate, is, setAlphabet, setId, setName, setParameterAtSite, setSiteSpecificParamIds, setSiteSpecificParamList, setWriteProtected, summary

 \bold{Methods inherited from PSRoot}:\cr
checkConsistency, enableVirtual, getComments, getMethodsList, globalConsistencyCheck, intersect.list, is.na, is, ll, my.all.equal, plot, setComments, setMethodsList, summary, virtualAssignmentForbidden

 \bold{Methods inherited from Object}:\cr
[[, [[<-, $, $<-, as.character, attach, attachLocally, clearCache, clearLookupCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFieldModifier, getFieldModifiers, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save

 }

 \examples{
	# create a GY94 object
	p<-GY94(kappa=2)
	# check if inherits from GY94
	is.GY94(p)
	# get object summary
	summary(p)
	# display a bubble plot
	plot(p)
	# create a codon sequence, attach process
	s<-CodonSequence(length=5, processes=list(list(p)))
	# sample states
	sampleStates(s)
	# make first three positions invariable
	setRateMultipliers(s,p,0,1:3)
	# sample omega values from the M3 (discrete) model.
	omegaVarM3(s,p,omegas=c(0,1,2,3),probs=c(2/5,1/5,1/5,1/5))
	# get a histogram of omega values in s
	omegaHist(s,p,breaks=50)
       sim<-PhyloSim(root.seq=s,phylo=rcoal(2))
       # run simulation
       Simulate(sim)
	# get the list of recorded per-branch event counts
	getBranchEvents(sim)
	# export the number of synonymous substitutions as a phylo object
	syn.subst<-exportStatTree(sim,"nr.syn.subst")
	syn.subst
	# plot the exported phylo object
	plot(syn.subst)
	# print alignment
	sim$alignment
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	CodonUNREST GeneralSubstitution CodonSequence GTR WAG
 }
