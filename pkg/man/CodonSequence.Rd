%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{CodonSequence}
\docType{class}
\alias{CodonSequence}


 \title{The CodonSequence class}

 \description{
	Sequence objects aggregating Site objects having a CodonAlphabet attached by default.

	Package:   \cr
\bold{Class CodonSequence}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link{PSRoot}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{\link{Sequence}}\cr
\code{~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~+--}\code{CodonSequence}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{CodonSequence}\cr
extends \link{Sequence}\cr


 }

 \usage{
CodonSequence(
    name=NA, string=NA, length=NA, 
    table.id=1, processes=NA, ancestral.obj=NA, ...
    )
}

 \arguments{
       \item{name}{Name of the Sequence object.}
       \item{string}{A string specifying the length and the states of the Sequence object.}
       \item{length}{The length of the sequence. Mutually exclusive with "string".}
	\item{table.id}{The genetic code table to use in the attached CodonAlphabet object ("Standard" by default).}
       \item{processes}{A list of lists of Process objects, to be attached to the aggregated Site objects. Recycled if shorter than the length of the sequence.}
       \item{ancestral.obj}{The ancestral object of the Sequence object (a valid Sequence or Process object).}
       \item{...}{Not used.}
	}

 \section{Fields and Methods}{
 	\bold{Methods:}\cr
\tabular{rll}{
 \tab \code{checkConsistency} \tab  -\cr
 \tab \code{getOmegas} \tab  -\cr
 \tab \code{omegaHist} \tab  -\cr
 \tab \code{omegaVarM0} \tab  -\cr
 \tab \code{omegaVarM1} \tab  -\cr
 \tab \code{omegaVarM2} \tab  -\cr
 \tab \code{omegaVarM3} \tab  -\cr
 \tab \code{omegaVarM4} \tab  -\cr
 \tab \code{setOmegas} \tab  -\cr
 \tab \code{Translate} \tab  -\cr
}


 \bold{Methods inherited from Sequence}:\cr
as.character, attachProcess, checkConsistency, clearStates, clone, copySubSequence, deleteSubSequence, detachProcess, getAlphabets, getAncestral, getBigRate, getCumulativeRates, getCumulativeRatesFromRange, getDeletionTolerance, getEvents, getId, getInsertionTolerance, getLength, getName, getOmegas, getParameterAtSites, getProcesses, getRateMultipliers, getSites, getStates, getString, getSymbolFreqs, getTotalRates, getTotalRatesFromRange, getUniqueAlphabets, getUniqueProcesses, getWriteProtected, insertSequence, is, plot, plotParametersAtSites, plusGamma, plusInvGamma, sampleStates, setAlphabets, setAncestral, setBigRate, setCumulativeRates, setDeletionTolerance, setId, setInsertionTolerance, setLength, setName, setOmegas, setParameterAtSites, setProcesses, setRateMultipliers, setStates, setString, setTotalRates, setUniqueAlphabets, setUniqueProcesses, setWriteProtected, summary

 \bold{Methods inherited from PSRoot}:\cr
checkConsistency, enableVirtual, getComments, getMethodsList, globalConsistencyCheck, intersect.list, is.na, is, ll, my.all.equal, plot, setComments, setMethodsList, summary, virtualAssignmentForbidden

 \bold{Methods inherited from Object}:\cr
[[, [[<-, $, $<-, as.character, attach, attachLocally, clearCache, clearLookupCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFieldModifier, getFieldModifiers, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save

 }

 \examples{
       # create an empty CodonSequence object
       s<-CodonSequence(length=50)
       s
       # set states
       s$states<-c("ATG","CGA","TTT","CTA")
       s
       # create a sequence object by specifying a string
       s<-CodonSequence(string="ATCTTTCGAATG")
       s
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{CodonSequence}}.
 }
