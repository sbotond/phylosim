%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{CodonSubst}
\docType{class}
\alias{CodonSubst}


 \title{The CodonSubst class}

 \description{
       This is a class implementing a continuous-time Markov process acting on
       the state-space defined by the \code{CodonAlphabet} class.

	The rate matrix can be built from PAML files specified by the \code{paml.file} argument.
	Alternatively the rates can be specified as a list through the \code{rate.list} parameter.

\bold{Class CodonSubst}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link{PSRoot}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{\link{Process}}\cr
\code{~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~+--}\code{\link{GeneralSubstitution}}\cr
\code{~~~~~~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~~~~~~+--}\code{CodonSubst}\cr

\bold{Directly known subclasses:}\cr
\link{ECMrest}, \link{ECMunrest}\cr

public static class \bold{CodonSubst}\cr
extends \link{GeneralSubstitution}\cr


 }

 \usage{
CodonSubst(name="Anonymous", paml.file=NA, rate.list=NA, equ.dist=NA, ...)
}

 \arguments{
 	\item{name}{The name of the object.}
	\item{paml.file}{The name of the PAML file used to construct the rate matrix.}
	\item{rate.list}{A list of unscaled substitution rates (see \code{setRateList.GeneralSubstitution}).}
	\item{equ.dist}{Equilibrium distribution.}
 	\item{...}{Additional arguments.}
 }

 \section{Fields and Methods}{
 	\bold{Methods:}\cr
\tabular{rll}{
 \tab \code{buildFromPAML} \tab  -\cr
 \tab \code{checkConsistency} \tab  -\cr
 \tab \code{newMatrix} \tab  -\cr
 \tab \code{setEquDist} \tab  -\cr
 \tab \code{summary} \tab  -\cr
}


 \bold{Methods inherited from GeneralSubstitution}:\cr
as.character, checkConsistency, clone, getAlphabet, getEquDist, getEventRate, getEventRateAtSite, getEventsAtSite, getQMatrix, getRate, getRateList, hasUndefinedRate, is, plot, rescaleQMatrix, sampleState, setAlphabet, setEquDist, setQMatrix, setRate, setRateList, summary

 \bold{Methods inherited from Process}:\cr
!=, ==, as.character, checkConsistency, clone, getAlphabet, getEventsAtSite, getId, getName, getParameterAtSite, getSiteSpecificParamIds, getSiteSpecificParamList, getWriteProtected, hasSiteSpecificParameter, hasUndefinedRate, is, setAlphabet, setId, setName, setParameterAtSite, setSiteSpecificParamIds, setSiteSpecificParamList, setWriteProtected, summary

 \bold{Methods inherited from PSRoot}:\cr
checkConsistency, enableVirtual, getComments, getMethodsList, globalConsistencyCheck, intersect.list, is, is.na, ll, my.all.equal, plot, setComments, setMethodsList, summary, virtualAssignmentForbidden

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clearLookupCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFieldModifier, getFieldModifiers, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, save


 }

 \examples{
	# create an object
	p<-CodonSubst()
	# build rate matrix from paml file
	# buildFromPAML(p,"path_to_paml_file")	# do not run this
	# get object summary
	summary(p)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{CodonSubst}}.
 }
