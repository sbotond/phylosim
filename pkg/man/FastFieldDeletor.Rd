%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{FastFieldDeletor}
\docType{class}
\alias{FastFieldDeletor}


 \title{The FastFieldDeletor class}

 \description{
	The \code{\link{DiscreteDeletor}} and \code{\link{ContinuousDeletor}} processes
	generate deletion event objects with rates determined by the general rate of the
	process and the "rate.multiplier" parameter. The probability of rejecting an event
	is determined by the product of the "deletion.tolerance" parameters from the affected sites.
	If the majority of the sites have low deletion tolerance most of the events are rejected, which
	slows down the simulation without performing much events.

	The \code{FastFieldDeletor} process scales down the rate and length distribution of the proposed
	events based on the highest insertion tolerance value observed in the root sequence
	(see the package vignette for details), thus making the simulation more efficient.

	The available length distributions are (see also the package vignette):
	\itemize{
		\item Geometric (default) - \code{lengthParam1} is \emph{Lambda}.
		\item Poisson+1 - \code{lengthParam1} is \emph{Lambda}.
		\item Conway-Maxwell Poisson+1 - \code{lengthParam1} is \emph{Lambda}, \code{lengthParam2} is \emph{nu}
		\item Negative Binomial+1 - \code{lengthParam1} is \emph{Lambda}, \code{lengthParam2} is \emph{r}
 	}

	Insertion proceses can insert sites with deletion tolerance higher than the largest
	deletion tolerance observed in the root sequence. The user can specify the largest expected
	tolerance value through the \code{toleranceMargin} virtual field. The process is then scaled by
	max(initial_highest_tolerance, \code{toleranceMargin}).

	Package:   \cr
\bold{Class FastFieldDeletor}\cr

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
\code{~~~~~~~~~~~~~~~~~~~~~~+--}\code{FastFieldDeletor}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{FastFieldDeletor}\cr
extends \link{GeneralDeletor}\cr


 }

 \usage{
FastFieldDeletor(
            name="Anonymous", type="geometric", 
            length.param.1=NA, length.param.2=NA, tolerance.margin=0, ...
            )
}

 \arguments{
 	\item{name}{Object name.}
 	\item{type}{Type of the process (see above).}
 	\item{length.param.1}{Object name.}
 	\item{length.param.2}{Object name.}
 	\item{tolerance.margin}{Object name.}
 	\item{...}{Additional arguments.}
	}

 \section{Fields and Methods}{
 	\bold{Methods:}\cr
\tabular{rll}{
 \tab \code{checkConsistency} \tab  -\cr
 \tab \code{getEventsAtSite} \tab  -\cr
 \tab \code{getLengthParam1} \tab  -\cr
 \tab \code{getLengthParam2} \tab  -\cr
 \tab \code{getToleranceMargin} \tab  -\cr
 \tab \code{getType} \tab  -\cr
 \tab \code{setLengthParam1} \tab  -\cr
 \tab \code{setLengthParam2} \tab  -\cr
 \tab \code{setParameterAtSite} \tab  -\cr
 \tab \code{setToleranceMargin} \tab  -\cr
 \tab \code{setType} \tab  -\cr
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
	# create a FastFieldDeletor object, default (geometric) type
	# length.param.1 is "lambda"
	p<-FastFieldDeletor(rate=1,length.param.1=0.9, tolerance.margin=0.8)
	# get type
	p$type
	# get object summary
	summary(p)
	# set/get lambda
	p$lengthParam1<-0.8
	p$lengthParam1
	# set/get tolerance margin
	p$toleranceMargin<-0.5
	p$toleranceMargin
	# create a nucleotide sequence, attach process
	s<-NucleotideSequence(length=30,processes=list(list(p)))
	# set state pattern
	s$states<-c("A","A","T","T","G","G","C","C")
	# sample deletion tolerances
	setDeletionTolerance(s,p,sample(seq(from=0,to=0.8,by=0.01),30,replace=TRUE))
	# plot deletion tolerance
	plotParametersAtSites(s,p,"deletion.tolerance")
	# simulate
	sim<-PhyloSim(root.seq=s, phylo=rcoal(2))
	Simulate(sim)
	# show alignment
	sim$alignment
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{FastFieldDeletor}}.
 }
