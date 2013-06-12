%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{GeneralInDel}
\docType{class}
\alias{GeneralInDel}


 \title{The GeneralInDel class}

 \description{

	This is a class implementing the methods which are needed by both the
	GeneralInsertor and GeneralDeletor process.

	Package:   \cr
\bold{Class GeneralInDel}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link{PSRoot}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{\link{Process}}\cr
\code{~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~+--}\code{GeneralInDel}\cr

\bold{Directly known subclasses:}\cr
\link{BrownianInsertor}, \link{ContinuousDeletor}, \link{ContinuousInsertor}, \link{DiscreteDeletor}, \link{DiscreteInsertor}, \link{FastFieldDeletor}, \link{GeneralDeletor}, \link{GeneralInsertor}\cr

public static class \bold{GeneralInDel}\cr
extends \link{Process}\cr


 }

 \usage{
GeneralInDel(name="Anonymous", rate=NA, propose.by=NA, accept.by=NA, ...)
}

 \arguments{
 	\item{name}{The name of the object.}
	\item{rate}{The general rate of the object.}
	\item{propose.by}{A function used to propose events.}
	\item{accept.by}{A function used to accept/reject events.}
 	\item{...}{Additional arguments.}
	}

 \section{Fields and Methods}{
 	\bold{Methods:}\cr
\tabular{rll}{
 \tab \code{checkConsistency} \tab  -\cr
 \tab \code{getAcceptBy} \tab  -\cr
 \tab \code{getProposeBy} \tab  -\cr
 \tab \code{getRate} \tab  -\cr
 \tab \code{hasUndefinedRate} \tab  -\cr
 \tab \code{is} \tab  -\cr
 \tab \code{proposeLength} \tab  -\cr
 \tab \code{setAcceptBy} \tab  -\cr
 \tab \code{setProposeBy} \tab  -\cr
 \tab \code{setRate} \tab  -\cr
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
	# create a GeneralInDel object
	# rejecting half of the events
	# and proposing sizes in the range 1:10
	o<-GeneralInDel(
                   rate=1,
                   propose.by=function(process){sample(1:10,1)},
                   accept.by=function(){sample(c(TRUE,FALSE),1)}
                   );
	# check if inherits from GeneralInDel
	is.GeneralInDel(o)
	# check if it has undefined rates
	hasUndefinedRate(o)
	# get object summary
	summary(o)
	# set/get proposeBy function via virtual field
	o$proposeBy<-function(process){return(3)}	# fixed event length
	o$proposeBy
	# set/get acceptBy function via virtual field
	o$acceptBy<-function(){return(TRUE)}		# accept all events
	o$acceptBy
	# get/set general rate
	o$rate
	o$rate<-2	# double the rate
	# propose event length
	proposeLength(o)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	Process GeneralInsertor GeneralDeletor GeneralSubstitution
 }
