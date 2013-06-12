%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{DiscreteDeletor}
\docType{class}
\alias{DiscreteDeletor}


 \title{The DiscreteDeletor class}

 \description{
	This class implements a process which performs deletions with
	lengths sampled from a user-specified discrete distribution.
	See \code{GeneralDeletor} for how the deletion process works.

	Package:   \cr
\bold{Class DiscreteDeletor}\cr

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
\code{~~~~~~~~~~~~~~~~~~~~~~+--}\code{DiscreteDeletor}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{DiscreteDeletor}\cr
extends \link{GeneralDeletor}\cr


 }

 \usage{
DiscreteDeletor(name="Anonymous", rate=NA, sizes=NA, probs=NA, ...)
}

 \arguments{
 	\item{name}{The name of the object.}
 	\item{rate}{The general rate.}
	\item{sizes}{The deletion sizes to propose.}
	\item{probs}{A vector with the probabilites of the deletion sizes.}
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
	# create a DiscreteDeletor process
	d<-DiscreteDeletor(
		name="M.D.",
		rate=0.25,
		sizes=c(1,2),
		probs=c(1/2,1/2)
 	)
	# get object summary
	summary(d)
	# set/get deletions sizes
	d$sizes<-1:3
	d$sizes
	# set/get length probabilities
	d$probs<-c(3,2,1)/6
	d$probs
	# plot length distribution
	plot(d)

	# The following code illustrates how to use
	# a DiscreteDeletor process in a simulation

	# create a sequence object, attach process d
	s<-NucleotideSequence(string="AAAAAAAAAAGGGGAAAAAAAAAA",processes=list(list(d)))
	# set the deletion tolerance to zero in the range 11:15
	# creating a region rejecting all deletions
	setDeletionTolerance(s,d,0,11:15)
	# get deletion tolerances
	getDeletionTolerance(s,d)
	# create a simulation object
	sim<-PhyloSim(root.seq=s,phylo=rcoal(2))
	# simulate
	Simulate(sim)
	# print resulting alignment
	sim$alignment
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	GeneralDeletor ContinuousDeletor GeneralInDel
 }
