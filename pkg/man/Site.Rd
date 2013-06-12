%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{Site}
\docType{class}
\alias{Site}


 \title{The Site class}

 \description{

	This is the class representing a site. Site objects can have one associated Alphabet object and one or
	more Process objects that act on their states.
	The associated Process and Site objects must have associated Alphabet objects with the same symbols set, or
	at least one of the Alphabet objects should inherit from the class AnyAlphabet.

	Site objects store the site-process-specific parameters of the attached Process objects.
	A site-process-specific parameter is a list containing: the identifier, the name, the value and type of the parameter.
	For example the ubiquitous rate multiplier site-process-specific parameter looks like
	\code{list(id="rate.multiplier",name="Rate multiplier",value=1,type="numeric")}.

	Templates for site-process-specific parameters and their default values are stored in the Process objects and
	copied into the Site object when the process is attached.

	Site objects have fields for associated ancestral Site objects and Sequence objects.

  	Package:   \cr
\bold{Class Site}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link{PSRoot}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{Site}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{Site}\cr
extends \link{PSRoot}\cr



 }

 \usage{
Site(state=NA, alphabet=NA, ancestral=NA, processes=NA, sequence=NA, ...)
}

 \arguments{
 	\item{state}{A symbol belonging to the specified alphabet.}
 	\item{alphabet}{An alphabet object.}
 	\item{ancestral}{The ancestral Site object.}
 	\item{processes}{A list of Process objects.}
 	\item{sequence}{The Sequence object to which the Site object belongs.}
	\item{...}{Not used.}
	}

 \section{Fields and Methods}{
               \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{as.character} \tab  -\cr
 \tab \code{attachProcess} \tab  -\cr
 \tab \code{checkConsistency} \tab  -\cr
 \tab \code{detachProcess} \tab  -\cr
 \tab \code{flagTotalRate} \tab  -\cr
 \tab \code{getAlphabet} \tab  -\cr
 \tab \code{getAncestral} \tab  -\cr
 \tab \code{getEvents} \tab  -\cr
 \tab \code{getProcesses} \tab  -\cr
 \tab \code{getSequence} \tab  -\cr
 \tab \code{getState} \tab  -\cr
 \tab \code{getTotalRate} \tab  -\cr
 \tab \code{is} \tab  -\cr
 \tab \code{isAttached} \tab  -\cr
 \tab \code{setAlphabet} \tab  -\cr
 \tab \code{setAncestral} \tab  -\cr
 \tab \code{setEvents} \tab  -\cr
 \tab \code{setProcesses} \tab  -\cr
 \tab \code{setSequence} \tab  -\cr
 \tab \code{setState} \tab  -\cr
 \tab \code{setTotalRate} \tab  -\cr
 \tab \code{summary} \tab  -\cr
}


 \bold{Methods inherited from PSRoot}:\cr
checkConsistency, enableVirtual, getComments, getMethodsList, globalConsistencyCheck, intersect.list, is.na, is, ll, my.all.equal, plot, setComments, setMethodsList, summary, virtualAssignmentForbidden

 \bold{Methods inherited from Object}:\cr
[[, [[<-, $, $<-, as.character, attach, attachLocally, clearCache, clearLookupCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFieldModifier, getFieldModifiers, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save

 }

 \examples{
		# create a site object
		site<-Site();
		# print the character representation (state)
		print(site);
		# get a summary
		summary(site);
		# create a site with a nucleotide alphabet
		site<-Site(state="A",alphabet=NucleotideAlphabet());
		# set site state
		site$state<-"G"
		# manipulate the associated Sequence object
		site$sequence<-Sequence()
		site$sequence
		# attach a substitution process
		site$processes<-list(JC69())
		# add one more substitution process
		attachProcess(site,K80())
		# get a list of active Event objects
		site$events
		# get a summary of the Site object
		summary(site);
 }

 \author{Botond Sipos, Gregory Jordan}


 \seealso{
		Alphabet, Process, Event
 }
