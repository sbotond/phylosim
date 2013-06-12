%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{GeneralSubstitution}
\docType{class}
\alias{GeneralSubstitution}


 \title{The GeneralSubstitution class}

 \description{
	This a class representing a continuous-time Markov process acting
	on the state space defined by the symbols stored in the Alphabet object
	passed to the object constructor.

	The GeneralSubstitution objects generate
	Event objects corresponding to substitution events based on the state of the
	attached Site objects.

	The GeneralSubstitution objects aggregate a QMatrix object, which stores the
	unscaled and scaled rate matrices. The scaled rate matrices, along with the
	site-process specific rate multiplier parameters define the rates of the generated
	Event objects.

	Package:   \cr
\bold{Class GeneralSubstitution}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link{PSRoot}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{\link{Process}}\cr
\code{~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~+--}\code{GeneralSubstitution}\cr

\bold{Directly known subclasses:}\cr
\link{AminoAcidSubst}, \link{BinarySubst}, \link{CodonUNREST}, \link{cpREV}, \link{F81}, \link{F84}, \link{GTR}, \link{GY94}, \link{HKY}, \link{JC69}, \link{JTT}, \link{JTT.dcmut}, \link{K80}, \link{K81}, \link{LG}, \link{mtArt}, \link{mtMam}, \link{mtREV24}, \link{MtZoa}, \link{PAM}, \link{PAM.dcmut}, \link{T92}, \link{TN93}, \link{UNREST}, \link{WAG}\cr

public static class \bold{GeneralSubstitution}\cr
extends \link{Process}\cr


 }

 \usage{
GeneralSubstitution(name="Anonymous", alphabet=NA, rate.list=NA, equ.dist=NA, ...)
}

 \arguments{
 	\item{name}{The name of the object.}
	\item{alphabet}{The alphabet on which the process acts (Alphabet object).}
	\item{rate.list}{A list with the substitution rates. It will be passed to \code{setRateList} method.}
	\item{equ.dist}{The equilibrium distribution (see \code{setEquDist.GeneralSubstitution}).}
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
checkConsistency, enableVirtual, getComments, getMethodsList, globalConsistencyCheck, intersect.list, is.na, is, ll, my.all.equal, plot, setComments, setMethodsList, summary, virtualAssignmentForbidden

 \bold{Methods inherited from Object}:\cr
[[, [[<-, $, $<-, as.character, attach, attachLocally, clearCache, clearLookupCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFieldModifier, getFieldModifiers, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save

 }

 \examples{
	# Create a GeneralSubstitution object
	# also provide an Alphabet objects
	# and the list of unscaled rates
	a<-BinaryAlphabet()
	gs<-GeneralSubstitution(
			name="MyBinarySubst",
			alphabet=a,
			rate.list=list("0->1"=2,"1->0"=1)
		)
	# check if inherits from GeneralSubstitution
	is.GeneralSubstitution(gs)
	# get an object summary
	summary(gs)
	# get a bubble plot
	plot(gs)
	# get unscaled rate for "0->1" by event name
	getRate(gs,"0->1")
	# get unscaled rate for "0->1" by states
	getRate(gs,from="0", to="1")
	# get scaled rate for "0->1" by name
	getEventRate(gs,"0->1")
	# get the list of unscaled event rates
	gs$rateList
	# set the \emph{unscaled} rates
	gs$rateList<-list("0->1"=1,"1->0"=1)
	# reset equilibrium distribution
	gs$equDist<- 5 * gs$equDist
	# get the equilibrium distribution
	gs$equDist
	# sample a state form the equilibrium distribution
	sampleState(gs)
	# get the associated QMatrix object
	gs$qMatrix
	# create a site object
	s<-Site(alphabet=a, state="0")
	# attach gs to s
	s$processes<-list(gs)
	# set rate multiplier for s and gs
	setParameterAtSite(gs,s,id="rate.multiplier",value=2)
	# get site specific rate for "0->1"
	getEventsAtSite(gs,s,"0->1")
	# get the list of active event objects given the state of s
	getEventsAtSite(gs,s)
	# get the associated Alphabet object
	gs$alphabet
	# clone the object
	gsc<-clone(gs)
	# modify the alphabet of gsc
	gsc$alphabet<-NucleotideAlphabet()
	summary(gsc)
	# check if gsc has undefined rates
	hasUndefinedRate(gsc)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	Process QMatrix Event Site GeneralIndel GTR WAG
 }
