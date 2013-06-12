%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{BinarySubst}
\docType{class}
\alias{BinarySubst}


 \title{The BinarySubst class}

 \description{
	This is a class implementing a continuous-time Markov process acting on
	the state space defined by the \code{BinaryAlphabet} class.

	Package:   \cr
\bold{Class BinarySubst}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link{PSRoot}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{\link{Process}}\cr
\code{~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~+--}\code{\link{GeneralSubstitution}}\cr
\code{~~~~~~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~~~~~~+--}\code{BinarySubst}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{BinarySubst}\cr
extends \link{GeneralSubstitution}\cr


 }

 \usage{
BinarySubst(name="Anonymous", rate.list=NA, equ.dist=NA, ...)
}

 \arguments{
 	\item{name}{The name of the object.}
 	\item{rate.list}{A list of substitution rates (see \code{setRateList.GeneralSubstitution}).}
	\item{equ.dist}{Equilibrium distribution.}
 	\item{...}{Additional arguments.}
	}

 \section{Fields and Methods}{
 	\bold{Methods:}\cr
\tabular{rll}{
 \tab \code{checkConsistency} \tab  -\cr
 \tab \code{summary} \tab  -\cr
}


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
 	# The following code demonstrates
       # the use of the BinarySubst process  
       # during a simulation.
       p<-BinarySubst(rate=0.25,name="Binary",rate.list=list("0->1"=2,"1->0"=1))
       # create a sequence object, attach process p
       s<-BinarySequence(string="000000000000000000",processes=list(list(p)));
       # make the first five positions invariable
       setRateMultipliers(s,p,0,1:5)
       # get rate multipliers
       getRateMultipliers(s,p)
       # simulate
       sim<-PhyloSim(root.seq=s,phylo=rcoal(3))
       Simulate(sim)
       # print alignment
       sim$alignment
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	GeneralSubstitution GTR WAG
 }
