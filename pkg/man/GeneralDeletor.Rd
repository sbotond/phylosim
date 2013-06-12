%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{GeneralDeletor}
\docType{class}
\alias{GeneralDeletor}


 \title{The GeneralDeletor class}

 \description{
	This is the class implementing a process generating deletion events.
	The rates of the deletion events are calculated as the product of the general rate
	of the process and the "rate.multiplier" site-process-specific parameter.

	The simulation code calls the \code{Perform} method on the selected
	deletion event objects,	which call their handler function to perform the deletion.

	The deletion lengths are proposed by the function stored in the \code{proposeBy} virtual field.
	The function must have the following arguments: process (the process object), sequence (the target sequence),
	position (the position of the site which generated the event).

	The deletion randomly affects the sites from the left or from the right of the target position (but never both).
	Positions which are out of range are discarded.

	The proposed deletions are accepted or rejected by the function stored in the \code{acceptBy} virtual field.
	The function must have the following arguments: process (the deletion prcoess), sequence (the target sequence), range (a vector of positions
	affected by the deletion).

	The probability of accepting a deletion is calculated as the product of the "deletion.tolerance" site-process-specific
	parameters from the sites affected by the deletion event.

	Package:   \cr
\bold{Class GeneralDeletor}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link{PSRoot}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{\link{Process}}\cr
\code{~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~+--}\code{\link{GeneralInDel}}\cr
\code{~~~~~~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~~~~~~+--}\code{GeneralDeletor}\cr

\bold{Directly known subclasses:}\cr
\link{ContinuousDeletor}, \link{DiscreteDeletor}, \link{FastFieldDeletor}\cr

public static class \bold{GeneralDeletor}\cr
extends \link{GeneralInDel}\cr


 }

 \usage{
GeneralDeletor(name="Anonymous", rate=NA, propose.by=NA, accept.by=NA, ...)
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
 \tab \code{getEventsAtSite} \tab  -\cr
 \tab \code{is} \tab  -\cr
 \tab \code{summary} \tab  -\cr
}


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
 	# create a GeneralDeletor object
       # proposed deletion length: 4, always accept
       d<-GeneralDeletor(
                       name = "DEL",
                       rate = 1,
                       propose.by=function(process, sequence, position){ 4 },
                       accept.by=function(process, sequence, range){ TRUE }
       )
	d
	# check if object inherits from GeneralDeletor
	is.GeneralDeletor(d)
	# get object summary
	summary(d)
	# set/get name
	d$name<-"Del Bosque"
	d$name
	# set/get rate
	d$rate<-0.5
	d$rate
	# set/get proposeBy
	# propose deletion lengths between 3 and 6
	d$proposeBy<-function(process, sequence, position){ sample(3:6,1) }
	d$proposeBy
	# set/get acceptBy
   # reject half of the events
	d$acceptBy<-function(process, sequence, range){ sample(c(TRUE, FALSE), 1)}
	d$acceptBy
	# create a sequence object, attach process
	s<-NucleotideSequence(string="AATTGGCCCCGGTTAA", processes=list(list(d)))
	# set the rate multiplier
       setRateMultipliers(s,d,2)
       # get the list of active events at site 6
       events<-getEventsAtSite(d,s$sites[[6]])
       events;
       # print sequence
       s
       # set the position for the event object
       e<-events[[1]];
       e$.position<-6;
       # perform the deletion event
       Perform(e)
       # check the results
       s
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	GeneralInDel DiscreteDeletor ContinuousDeletor FastFieldDeletor
 }
