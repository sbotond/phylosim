%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{F81}
\docType{class}
\alias{F81}


 \title{The F81 class}

 \description{
	This class implements the F81 GTR-submodel.

	Package:   \cr
\bold{Class F81}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link{PSRoot}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{\link{Process}}\cr
\code{~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~+--}\code{\link{GeneralSubstitution}}\cr
\code{~~~~~~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~~~~~~+--}\code{\link{UNREST}}\cr
\code{~~~~~~~~~~~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~~~~~~~~~~~+--}\code{\link{GTR}}\cr
\code{~~~~~~~~~~~~~~~~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~~~~~~~~~~~~~~~~+--}\code{F81}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{F81}\cr
extends \link{GTR}\cr


 }
 \references{
 Felsenstein, J (1981) Evolutionary trees from DNA sequences: a maximum likelihood approach -
 Journal of Molecular Evolution 17:368-376 \url{http://dx.doi.org/10.1007/BF01734359}
 }

 \usage{
F81(name="Anonymous", base.freqs=c(0.25, 0.25, 0.25, 0.25), ...)
}

 \arguments{
 	\item{name}{Object name.}
	\item{base.freqs}{Base frequency parameters.}
 	\item{...}{Not used.}
	}

 \section{Fields and Methods}{
 	\bold{Methods:}\cr
\tabular{rll}{
 \tab \code{checkConsistency} \tab  -\cr
 \tab \code{getBaseFreqs} \tab  -\cr
 \tab \code{getRateParam} \tab  -\cr
 \tab \code{getRateParamList} \tab  -\cr
 \tab \code{setBaseFreqs} \tab  -\cr
 \tab \code{setRateParam} \tab  -\cr
 \tab \code{setRateParamList} \tab  -\cr
 \tab \code{summary} \tab  -\cr
}


 \bold{Methods inherited from GTR}:\cr
checkConsistency, getBaseFreqs, getRateParam, getRateParamList, setBaseFreqs, setRateParam, setRateParamList, summary

 \bold{Methods inherited from UNREST}:\cr
checkConsistency, summary

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
	# create substitution process object
       p<-F81(base.freqs=c(1,2,3,4)/10)
       # get a summary
       summary(p)
	# display a bubble plot
	plot(p)

	# The following code demonstrates how to use
	# the process in a simulation.

	# create a sequence, attach process p
	s<-NucleotideSequence(length=20,processes=list(list(p)))
	# sample states
	sampleStates(s)
	# make the first five positions invariable
	setRateMultipliers(s,p,0,1:5)
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
 	GTR UNREST GeneralSubstitution HKY
 }
