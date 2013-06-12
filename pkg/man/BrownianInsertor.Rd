%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{BrownianInsertor}
\docType{class}
\alias{BrownianInsertor}


 \title{The BrownianInsertor class}

 \description{
	The \code{BrownianInsertor} class inherits from the \code{DiscreteInsertor}
	or \code{ContinuousInsertor} class depending on the \code{type} constructor argument
	("discrete" or "continuous").

	This process generates the insert sequence based on the sites flanking the insertions as follows:
	\itemize{
		\item An insert length is sampled by calling the function stored in the \code{proposeBy} virtual field.
		\item A sequence object is constructed.
		\item The processes attached to both flanking sites are attached to the insert sequence. If there are no common processes, the processes from a randomly chosen site will be attached to the insert.
		\item The site-process specific parameters are sampled from Brownian paths with linear trends having the values from the flanking sites as endpoints.
 	}

	The "noisiness" of the Brownian path can be controlled through the \code{scale} virtual field/constructor parameter.

	Package:   \cr
\bold{Class BrownianInsertor}\cr

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
\code{~~~~~~~~~~~~~~~~~~~~~~+--}\code{\link{DiscreteInsertor}}\cr
\code{~~~~~~~~~~~~~~~~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~~~~~~~~~~~~~~~~+--}\code{BrownianInsertor}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{BrownianInsertor}\cr
extends \link{DiscreteInsertor}\cr


 }

 \usage{
BrownianInsertor(name="Anonymous", type="discrete", scale=0.001, ...)
}

 \arguments{
 	\item{name}{Object name.}
	\item{type}{Process type (see above).}
	\item{scale}{Brownian path scale parameter.}
 	\item{...}{Additional arguments.}
	}

 \section{Fields and Methods}{
 	\bold{Methods:}\cr
\tabular{rll}{
 \tab \code{BrownianPath} \tab  -\cr
 \tab \code{checkConsistency} \tab  -\cr
 \tab \code{getScale} \tab  -\cr
 \tab \code{getType} \tab  -\cr
 \tab \code{setScale} \tab  -\cr
 \tab \code{setType} \tab  -\cr
 \tab \code{summary} \tab  -\cr
}


 \bold{Methods inherited from DiscreteInsertor}:\cr
checkConsistency, getProbs, getSizes, plot, setProbs, setSizes, summary

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
	# create a BrownianInsertor process, discrete type
	p<-BrownianInsertor(
                       type="discrete",
                       scale=0.05,
                       sizes=1:4,
                       probs=c(3/6,1/6,1/6,1/6),
                       rate=0.05
                       )
	# get object summary
	summary(p)
	# plot insert length distribution
	plot(p)
	# create a nucleotide sequence, attach processes
	s<-NucleotideSequence(string="AAAAAAAAAAA",processes=list(list(p,JC69())))
	# create simulation object
	sim<-PhyloSim(root.seq=s, phylo=rcoal(2))
	# simulate and show alignment
	Simulate(sim)
	sim$alignment
	# check the rate multipliers and insertion tolerances in one of the sequences
	res<-sim$sequences[[2]]
	getRateMultipliers(res,p)
	getInsertionTolerance(res,p)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	DiscreteInsertor ContinuousInsertor GeneralInsertor GeneralInDel
 }
