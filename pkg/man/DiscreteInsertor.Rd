%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{DiscreteInsertor}
\docType{class}
\alias{DiscreteInsertor}


 \title{The DiscreteInsertor class}

 \description{
       This class implements a process which performs insertions with
       lengths sampled from a user-specified discrete distribution.
       See \code{GeneralInsertor} for how the insertion process works.

       Package:   \cr
\bold{Class DiscreteInsertor}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link{PSRoot}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{\link{Process}}\cr
\code{~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~+--}\code{\link{GeneralInDel}}\cr
\code{~~~~~~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~~~~~~+--}\code{\link{GeneralInsertor}}\cr
\code{~~~~~~~~~~~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~~~~~~~~~~~+--}\code{DiscreteInsertor}\cr

\bold{Directly known subclasses:}\cr
\link{BrownianInsertor}\cr

public static class \bold{DiscreteInsertor}\cr
extends \link{GeneralInsertor}\cr


 }
     
 \usage{
DiscreteInsertor(name="Anonymous", rate=NA, sizes=NA, probs=NA, ...)
}
     
 \arguments{
       \item{name}{The name of the object.}
       \item{rate}{The general rate.}
       \item{sizes}{The insertion sizes to propose.}
       \item{probs}{A vector with the probabilites of the insertion sizes.}
       \item{...}{Additional arguments.}
       }

 \section{Fields and Methods}{
       \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{checkConsistency} \tab  -\cr
 \tab \code{getProbs} \tab  -\cr
 \tab \code{getSizes} \tab  -\cr
 \tab \code{plot} \tab  -\cr
 \tab \code{setProbs} \tab  -\cr
 \tab \code{setSizes} \tab  -\cr
 \tab \code{summary} \tab  -\cr
}


 \bold{Methods inherited from GeneralInsertor}:\cr
checkConsistency, generateInsert, getAcceptWin, getEventsAtSite, getGenerateBy, getInsertHook, getTemplateSeq, is, setAcceptWin, setGenerateBy, setInsertHook, setTemplateSeq, summary

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
       # create a DiscreteInsertor process
       i<-DiscreteInsertor(
               name="Mii",
               rate=0.25,
               sizes=c(1,2),
               probs=c(1/2,1/2)
       )
	# set template sequence
	i$templateSeq<-NucleotideSequence(string="C")
       # get object summary
       summary(i)
       # set/get insertion sizes
       i$sizes<-1:3
	i$sizes
       # set/get length probabilities
       i$probs<-c(3,2,1)/6
       i$probs
       # plot length distribution
       plot(i)
     
       # The following code illustrates how to use
       # a DiscreteInsertor process in a simulation
     
       # create a sequence object and attach process i to it
       s<-NucleotideSequence(string="AAAAAAAAAAGGGGAAAAAAAAAA",processes=list(list(i)))
       # set the insertion tolerance to zero in range 11:15
       # creating a region rejecting all insertions
       setInsertionTolerance(s,i,0,11:15)     
       # get insertion tolerances
       getInsertionTolerance(s,i)
       # create a simulation object
       sim<-PhyloSim(root.seq=s,phylo=rcoal(2))
       # simulate
       Simulate(sim)
       # print resulting alignment
       sim$alignment
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
       GeneralInsertor ContinuousInsertor GeneralInDel
 }
