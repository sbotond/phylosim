%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{GTR}
\docType{class}
\alias{GTR}


 \title{The GTR class}

 \description{
	This class implements the general time-reversible nucleotide substitution model (GTR, REV).
	The rate parameters are named as in PAML (see PAML documentation: \url{http://bit.ly/9SQK2f}).

	The default value for the rate parameters is 1 and the default value for the base
	frequencies is 0.25. So the GTR objects are equivalent to JC69 objects by default.

	Package:   \cr
\bold{Class GTR}\cr

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
\code{~~~~~~~~~~~~~~~~~~~~~~+--}\code{GTR}\cr

\bold{Directly known subclasses:}\cr
\link{F81}, \link{F84}, \link{HKY}, \link{K80}, \link{K81}, \link{T92}, \link{TN93}\cr

public static class \bold{GTR}\cr
extends \link{UNREST}\cr


 }
 \references{
	Tavare, S (1986) "Some Probabilistic and Statistical Problems in the Analysis of DNA Sequences".
	American Mathematical Society: Lectures on Mathematics in the Life Sciences 17:57-86
 }

 \usage{
GTR(
        name="Anonymous", rate.params=list(a = 1, b = 1, c = 1, d = 1, e = 1, f = 1),
        base.freqs=rep(0.25, times = 4), ...
        )
}

 \arguments{
 	\item{name}{Object name.}
	\item{rate.params}{A list of unscaled rates (see \code{setRateList.GeneralSubstitution}).}
	\item{base.freqs}{Equlibrium distribution.}
 	\item{...}{Additional arguments.}
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
       p<-GTR(
		rate.params=list(
			"a"=1, "b"=2, "c"=3,
			"d"=1, "e"=2, "f"=3
		),
		base.freqs=c(2,2,1,1)/6
	)
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
	# create a simulation object and run simulation
	sim<-PhyloSim(root.seq=s,phylo=rcoal(2))
	Simulate(sim)
	# print alignment
	sim$alignment
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	GeneralSubstitution UNREST HKY
 }
