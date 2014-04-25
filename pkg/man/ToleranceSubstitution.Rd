%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{ToleranceSubstitution}
\docType{class}
\alias{ToleranceSubstitution}


 \title{The ToleranceSubstitution class}

 \description{
	This a class representing a continuous-time Markov process acting
	on the state space defined by the symbols stored in the Alphabet object
	passed to the object constructor.

   In contrast to GeneralSubstitution, the ToleranceSubstitution class has a site-specific
   substitution tolerance parameter ("substitution.tolerance") which determines the probability of
   accepting the proposed events. As a consequence, the branch lengths inferred from
   the simulated data will no longer correspond to the neutral expectations unless
   all tolerance values are equal to 1.

	Package:  \cr
\bold{Class ToleranceSubstitution}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link{PSRoot}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{\link{Process}}\cr
\code{~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~+--}\code{ToleranceSubstitution}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{ToleranceSubstitution}\cr
extends \link{Process}\cr


 }

 \usage{
ToleranceSubstitution(name="Anonymous", alphabet=NA, rate.list=NA, equ.dist=NA, ...)
}

 \arguments{
 	\item{name}{The name of the object.}
	\item{alphabet}{The alphabet on which the process acts (Alphabet object).}
	\item{rate.list}{A list with the substitution rates. It will be passed to \code{setRateList} method.}
	\item{equ.dist}{The equilibrium distribution (see \code{setEquDist.ToleranceSubstitution}).}
 	\item{...}{Not used.}
	}

 \section{Fields and Methods}{
 	\bold{Methods:}\cr
\tabular{rll}{
 \tab \code{as.character} \tab  -\cr
 \tab \code{checkConsistency} \tab  -\cr
 \tab \code{clone} \tab  -\cr
 \tab \code{getAlphabet} \tab  -\cr
 \tab \code{getEquDist} \tab  -\cr
 \tab \code{getEventRate} \tab  -\cr
 \tab \code{getEventRateAtSite} \tab  -\cr
 \tab \code{getEventsAtSite} \tab  -\cr
 \tab \code{getQMatrix} \tab  -\cr
 \tab \code{getRate} \tab  -\cr
 \tab \code{getRateList} \tab  -\cr
 \tab \code{hasUndefinedRate} \tab  -\cr
 \tab \code{is} \tab  -\cr
 \tab \code{plot} \tab  -\cr
 \tab \code{rescaleQMatrix} \tab  -\cr
 \tab \code{sampleState} \tab  -\cr
 \tab \code{setAlphabet} \tab  -\cr
 \tab \code{setEquDist} \tab  -\cr
 \tab \code{setQMatrix} \tab  -\cr
 \tab \code{setRate} \tab  -\cr
 \tab \code{setRateList} \tab  -\cr
 \tab \code{summary} \tab  -\cr
}


 \bold{Methods inherited from Process}:\cr
!=, ==, as.character, checkConsistency, clone, getAlphabet, getEventsAtSite, getId, getName, getParameterAtSite, getSiteSpecificParamIds, getSiteSpecificParamList, getWriteProtected, hasSiteSpecificParameter, hasUndefinedRate, is, setAlphabet, setId, setName, setParameterAtSite, setSiteSpecificParamIds, setSiteSpecificParamList, setWriteProtected, summary

 \bold{Methods inherited from PSRoot}:\cr
checkConsistency, enableVirtual, getComments, getMethodsList, globalConsistencyCheck, intersect.list, is, is.na, ll, my.all.equal, plot, setComments, setMethodsList, summary, virtualAssignmentForbidden

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clearLookupCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFieldModifier, getFieldModifiers, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, save


 }

 \examples{
 # construct a GTR process object, we will use this to fill in the rates
 # for the ToleranceSubstitution process.
 gtr         <- GTR(
       name="MyGTR",
           rate.params=list(
               "a"=1, "b"=2, "c"=3,
                   "d"=1, "e"=2, "f"=3
           ),
           base.freqs=c(2,2,1,1)/6
   )
 rate.list.gtr   <- gtr$rateList

 # Construct the ToleranceSubstitution process.
 p   <- ToleranceSubstitution(
     name        = "MyTolSubst",
     alphabet    = NucleotideAlphabet(),
     rate.list   = rate.list.gtr,
 )

 plot(p)

 # construct root sequence object
 s<-NucleotideSequence(length=20)

 # attach process
 attachProcess(s,p)

 # sample states from the equilibrium
 # distribution of the attached processes
 sampleStates(s)

 ## Set the substitution tolerance parameters for some sites:
 setParameterAtSites(s, p, "substitution.tolerance",c(0,0.05,0.1),1:3)

 ## Plot the substitution tolerance parameters across sites:
 plotParametersAtSites(s,p,"substitution.tolerance")

 # Construct simulation object:
 sim <-PhyloSim(root.seq=s, phylo=rtree(3))

 # Run simulation:
 Simulate(sim)

 # Plot alignment:
 plot(sim)

 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	Process QMatrix Event Site GeneralIndel GTR WAG
 }
