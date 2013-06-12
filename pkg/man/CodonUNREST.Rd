%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{CodonUNREST}
\docType{class}
\alias{CodonUNREST}


 \title{The CodonUNREST class}

 \description{
	This class implements a time-continuous Markov process acting on a state
	space defined by the symbol set of a CodonAlphabet object. The rate matrix of this model
	is unrestricted, so it can be used to implement empircal codon models or more
	restricted mechanistic models.

	Package:   \cr
\bold{Class CodonUNREST}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link{PSRoot}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{\link{Process}}\cr
\code{~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~+--}\code{\link{GeneralSubstitution}}\cr
\code{~~~~~~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~~~~~~+--}\code{CodonUNREST}\cr

\bold{Directly known subclasses:}\cr
\link{GY94}\cr

public static class \bold{CodonUNREST}\cr
extends \link{GeneralSubstitution}\cr


 }

 \usage{
CodonUNREST(name="Anonymous", table.id=1, rate.list=NA, equ.dist=NA, ...)
}

 \arguments{
 	\item{name}{The name of the object.}
 	\item{table.id}{The identifier of the genetic code table (see \code{CodonAlphabet}).}
 	\item{rate.list}{A list of unscaled substitution rates (see \code{setRateList.GeneralSubstitution}).}
 	\item{equ.dist}{Equilibrium distribution.}
 	\item{...}{Additional arguments.}
	}

 \section{Fields and Methods}{
 	\bold{Methods:}\cr
\tabular{rll}{
 \tab \code{checkConsistency} \tab  -\cr
 \tab \code{is} \tab  -\cr
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
	# create a CodonUNREST object
	p<-CodonUNREST(table.id=2)
	# get object summary
	summary(p)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	GeneralSubstitution GY94
 }
