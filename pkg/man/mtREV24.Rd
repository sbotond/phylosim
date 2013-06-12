%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{mtREV24}
\docType{class}
\alias{mtREV24}


 \title{The mtREV24 empirical amino acid substitution model}

 \description{


	Package:   \cr
\bold{Class mtREV24}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link{PSRoot}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{\link{Process}}\cr
\code{~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~+--}\code{\link{GeneralSubstitution}}\cr
\code{~~~~~~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~~~~~~+--}\code{\link{AminoAcidSubst}}\cr
\code{~~~~~~~~~~~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~~~~~~~~~~~+--}\code{mtREV24}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{mtREV24}\cr
extends \link{AminoAcidSubst}\cr


 }

 \references{
 Adachi, J. and Hasegawa, M. (1996) MOLPHY version 2.3: programs for
 molecular phylogenetics based on maximum likelihood.  Computer Science
 Monographs of Institute of Statistical Mathematics 28:1-150
 }

 \usage{
mtREV24(equ.dist=NA, ...)
}

 \arguments{
 	\item{equ.dist}{Equilibrium distribution.}
 	\item{...}{Not used.}
	}

 \section{Fields and Methods}{
 	\bold{Methods:}\cr
\emph{No methods defined}.


 \bold{Methods inherited from AminoAcidSubst}:\cr
buildFromPAML, checkConsistency, newAAMatrix, setEquDist, summary

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
	# create substitution model object
	p<-mtREV24()
	# get object summary
	summary(p)
	# display a bubble plot
	plot(p)

       # The following code demonstrates how to use
       # the process in a simulation.

	# create a sequence, attach process p
	s<-AminoAcidSequence(length=10,processes=list(list(p)) )
	# sample states
	sampleStates(s)
       # make the first three positions invariable
       setRateMultipliers(s,p,0,1:3)
       # get rate multipliers
       getRateMultipliers(s,p)
       # create a simulation object
       sim<-PhyloSim(root.seq=s,phylo=rcoal(2))
       # run simulation
       Simulate(sim)
       # print alignment
       sim$alignment
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	AminoAcidSubst GeneralSubstitution UNREST
 }
