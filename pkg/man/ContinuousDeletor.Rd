%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{ContinuousDeletor}
\docType{class}
\alias{ContinuousDeletor}


 \title{The ContinuousDeletor class}

 \description{
	This class implements a process which performs deletions with
       lengths sampled from a user-specified R expression returning a
	numeric value.
       See \code{GeneralDeletor} for the how the deletion processes
	works.

	Package:   \cr
\bold{Class ContinuousDeletor}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link{PSRoot}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{\link{Process}}\cr
\code{~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~+--}\code{\link{GeneralInDel}}\cr
\code{~~~~~~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~~~~~~+--}\code{\link{GeneralDeletor}}\cr
\code{~~~~~~~~~~~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~~~~~~~~~~~+--}\code{ContinuousDeletor}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{ContinuousDeletor}\cr
extends \link{GeneralDeletor}\cr


 }

 \usage{
ContinuousDeletor(name="Anonymous", rate=NA, dist=NA, max.length=NA, ...)
}

 \arguments{
 	\item{name}{The name of the object.}
 	\item{rate}{The general rate.}
 	\item{dist}{The length sampling expression.}
 	\item{max.length}{Maximum event length.}
 	\item{...}{Additional arguments.}
	}

 \section{Fields and Methods}{
 	\bold{Methods:}\cr
\tabular{rll}{
 \tab \code{checkConsistency} \tab  -\cr
 \tab \code{getDist} \tab  -\cr
 \tab \code{getMaxLength} \tab  -\cr
 \tab \code{plot} \tab  -\cr
 \tab \code{setDist} \tab  -\cr
 \tab \code{setMaxLength} \tab  -\cr
 \tab \code{summary} \tab  -\cr
}


 \bold{Methods inherited from GeneralDeletor}:\cr
checkConsistency, getEventsAtSite, is, summary

 \bold{Methods inherited from GeneralInDel}:\cr
checkConsistency, getAcceptBy, getProposeBy, getRate, hasUndefinedRate, is, proposeLength, setAcceptBy, setProposeBy, setRate, summary

 \bold{Methods inherited from Process}:\cr
!=, ==, as.character, checkConsistency, clone, getAlphabet, getEventsAtSite, getId, getName, getParameterAtSite, getSiteSpecificParamIds, getSiteSpecificParamList, getWriteProtected, hasSiteSpecificParameter, hasUndefinedRate, is, setAlphabet, setId, setName, setParameterAtSite, setSiteSpecificParamIds, setSiteSpecificParamList, setWriteProtected, summary

 \bold{Methods inherited from PSRoot}:\cr
checkConsistency, enableVirtual, getComments, getMethodsList, globalConsistencyCheck, intersect.list, is.na, is, ll, my.all.equal, plot, setComments, setMethodsList, summary, virtualAssignmentForbidden

 \bold{Methods inherited from Object}:\cr
[[, [[<-, $, $<-, as.character, attach, attachLocally, clearCache, clearLookupCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFieldModifier, getFieldModifiers, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save

 }

 \examples{
 	# create a ContinuousDeletor process
       o<-ContinuousDeletor(
               name="Conty",
               rate=0.25,
		dist=expression(1),
		max.length=2
       )
       # get object summary
       summary(o)
       # set/get length sampling expression
       o$dist<-expression(rnorm(1,mean=3,sd=3))
	o$dist
       # set/get maximum event length
	o$maxLength<-4
	o$maxLength
       # plot length density
       plot(o)
     
       # The following code illustrates how to use
       # a ContinuousDeletor process in a simulation
     
       # create a sequence object, attach process o
       s<-NucleotideSequence(string="AAAAAAAAAAGGGGAAAAAAAAAA",processes=list(list(o)))
       # set the deletion tolerance to zero in range 11:15
       # creating a region rejecting all deletions
       setDeletionTolerance(s,o,0,11:15)     
       # get deletion tolerances
       getDeletionTolerance(s,o)
       # create a simulation object
       sim<-PhyloSim(root.seq=s,phylo=rcoal(2))
       # simulate
       Simulate(sim)
       # print resulting alignment
       sim$alignment
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	GeneralDeletor DiscreteDeletor GeneralInDel
 }
