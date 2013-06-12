%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{Sequence}
\docType{class}
\alias{Sequence}


 \title{The Sequence class}

 \description{

	This is the class representing a sequence. The backbone of the Sequence objects are
	lists aggregating Site objects. The class has fields for keeping track of cumulative
	site rates, the sum of all active event rates and methods for performing actions
	on a collection of sites (positions).

	The Sequence objects have a field specifying an ancestral object, which can be a Sequence
	object (when the object is obtained through clone() ) or the "Root insertion process" object
	(for newly created objects).

	Package:   \cr
\bold{Class Sequence}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link{PSRoot}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{Sequence}\cr

\bold{Directly known subclasses:}\cr
\link{AminoAcidSequence}, \link{BinarySequence}, \link{CodonSequence}, \link{NucleotideSequence}\cr

public static class \bold{Sequence}\cr
extends \link{PSRoot}\cr


 }

 \usage{
Sequence(name=NA, string=NA, length=NA, alphabets=NA, processes=NA, ancestral.obj=NA, ...)
}

 \arguments{
	\item{name}{The name of the Sequence object.}
 	\item{string}{A string containing symbols belonging to the associated Alphabet object.
	It can be used to set the initial states of the aggregated Site objects. It also specifies the length of the sequence}
 	\item{length}{The length of the sequence. It cannot be used when 'string' is specified.}
 	\item{alphabets}{A list of Alphabet objects to be associated with the Site objects.
	The list is recycled in the case it is shorter than the sequence length.}
 	\item{processes}{A list of lists of Process objects to be attached
	(recycled if shorter than sequence length). }
 	\item{ancestral.obj}{The ancestral object (Sequence or Process).}
 	\item{...}{Not used.}
	}

 \section{Fields and Methods}{
 	\bold{Methods:}\cr
\tabular{rll}{
 \tab \code{as.character} \tab  -\cr
 \tab \code{attachProcess} \tab  -\cr
 \tab \code{checkConsistency} \tab  -\cr
 \tab \code{clearStates} \tab  -\cr
 \tab \code{clone} \tab  -\cr
 \tab \code{copySubSequence} \tab  -\cr
 \tab \code{deleteSubSequence} \tab  -\cr
 \tab \code{detachProcess} \tab  -\cr
 \tab \code{getAlphabets} \tab  -\cr
 \tab \code{getAncestral} \tab  -\cr
 \tab \code{getBigRate} \tab  -\cr
 \tab \code{getCumulativeRates} \tab  -\cr
 \tab \code{getCumulativeRatesFromRange} \tab  -\cr
 \tab \code{getDeletionTolerance} \tab  -\cr
 \tab \code{getEvents} \tab  -\cr
 \tab \code{getId} \tab  -\cr
 \tab \code{getInsertionTolerance} \tab  -\cr
 \tab \code{getLength} \tab  -\cr
 \tab \code{getName} \tab  -\cr
 \tab \code{getOmegas} \tab  -\cr
 \tab \code{getParameterAtSites} \tab  -\cr
 \tab \code{getProcesses} \tab  -\cr
 \tab \code{getRateMultipliers} \tab  -\cr
 \tab \code{getSites} \tab  -\cr
 \tab \code{getStates} \tab  -\cr
 \tab \code{getString} \tab  -\cr
 \tab \code{getSymbolFreqs} \tab  -\cr
 \tab \code{getTotalRates} \tab  -\cr
 \tab \code{getTotalRatesFromRange} \tab  -\cr
 \tab \code{getUniqueAlphabets} \tab  -\cr
 \tab \code{getUniqueProcesses} \tab  -\cr
 \tab \code{getWriteProtected} \tab  -\cr
 \tab \code{insertSequence} \tab  -\cr
 \tab \code{is} \tab  -\cr
 \tab \code{plot} \tab  -\cr
 \tab \code{plotParametersAtSites} \tab  -\cr
 \tab \code{plusGamma} \tab  -\cr
 \tab \code{plusInvGamma} \tab  -\cr
 \tab \code{sampleStates} \tab  -\cr
 \tab \code{setAlphabets} \tab  -\cr
 \tab \code{setAncestral} \tab  -\cr
 \tab \code{setBigRate} \tab  -\cr
 \tab \code{setCumulativeRates} \tab  -\cr
 \tab \code{setDeletionTolerance} \tab  -\cr
 \tab \code{setId} \tab  -\cr
 \tab \code{setInsertionTolerance} \tab  -\cr
 \tab \code{setLength} \tab  -\cr
 \tab \code{setName} \tab  -\cr
 \tab \code{setOmegas} \tab  -\cr
 \tab \code{setParameterAtSites} \tab  -\cr
 \tab \code{setProcesses} \tab  -\cr
 \tab \code{setRateMultipliers} \tab  -\cr
 \tab \code{setStates} \tab  -\cr
 \tab \code{setString} \tab  -\cr
 \tab \code{setTotalRates} \tab  -\cr
 \tab \code{setUniqueAlphabets} \tab  -\cr
 \tab \code{setUniqueProcesses} \tab  -\cr
 \tab \code{setWriteProtected} \tab  -\cr
 \tab \code{summary} \tab  -\cr
}


 \bold{Methods inherited from PSRoot}:\cr
checkConsistency, enableVirtual, getComments, getMethodsList, globalConsistencyCheck, intersect.list, is.na, is, ll, my.all.equal, plot, setComments, setMethodsList, summary, virtualAssignmentForbidden

 \bold{Methods inherited from Object}:\cr
[[, [[<-, $, $<-, as.character, attach, attachLocally, clearCache, clearLookupCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFieldModifier, getFieldModifiers, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save

 }

 \examples{
	# create a sequence object by
	# providng alphabets, processes and states
	s.one<-Sequence(
		name="Seq",
		string="AATTGGCCTTAAGGCCTTAA",
		alphabets=list(NucleotideAlphabet()),
		processes=list(list(JC69()))
	)
	s.one
	# check if inherits from Sequence
	is.Sequence(s.one)
	# get object summary
	summary(s.one)
	# create a sequence object,
	# specifying length, alphabets
	# and ancestral object
	s<-Sequence(
		name="Seq",
		length=20,
		ancestral.obj=s.one
	)
	# get sequence string
	s$string
	# get the list of site objects
	s$sites
	# get object id
	s$id
	# set and get name
	s$name<-"SeqSeq"
	s$seq
	# get length
	s$length
	# get and set ancestral object
	s$ancestral
	s$ancestral<-Sequence();
	# set alphabets
	setAlphabets(s,list(NucleotideAlphabet()))
	# set states
	# "A"-s in the range 1:10
	setStates(s,"A",1:10)
	# a pattern of "ATGC"-s in the range 11:20
	setStates(s,c("A","T","G","C"),11:20)
	# get states from range 10:12
	getStates(s,10:12)
	# attach a JC69 process to range 1:10
	jc69<-JC69()
	attachProcess(s,jc69,1:10)
	# set the rate multiplier site-process specific parameter for jc69
	setParameterAtSites(s,jc69,"rate.multiplier",2,1:10)
	# get "rate.multiplier" for jc69 from range 1:2
	getParameterAtSites(s, jc69, "rate.multiplier",1:2)
	# attach a GTR process to range 11:20
	gtr<-GTR()
	attachProcess(s,gtr,11:20)
	# set and get rate multipliers for gtr
	setRateMultipliers(s, gtr, c(0,5,1), 11:20)
	getRateMultipliers(s, gtr, 11:20)
	# get processes from range 1:5
	getProcesses(s,1:5)
	# replace the processes with a (GTR, JC69), JC69 pattern
	s$processes<-list(list(GTR(), JC69()), list(JC69()))
	# get processes from range 1:2
	getProcesses(s,1:2)
	# get unique processes
	s$uniqueProcesses
	# get unique alphabets
	s$uniqueAlphabets
	# get symbol frequencies
	getSymbolFreqs(s)
	# get the big rate
	s$bigRate
	# get a vector of total rates from range 1:4
	getTotalRatesFromRange(s,1:4)
	# get a vector of cumulative rates from range 1:4
	getCumulativeRatesFromRange(s,1:4)
	# reset all site states
	clearStates(s)
	s
	# sample states from the equilibrium distributions
	# of the attached substitution processes
	sampleStates(s)
	s
	# clone s
	s.clone<-clone(s)
	# insert a sequence in s.clone after position 2
	insertSequence(s.clone,NucleotideSequence(string="AAAAAA"),2)
	s.clone
	# delete positions 1,2,3 and 10
	deleteSubSequence(s.clone, c(1:3,10))
	s.clone
	# copy positions 7:10 into a new sequence object
	sc<-copySubSequence(s.clone, 7:10)
	sc
	# get events from sc in the range 1:2
	getEvents(sc,1:2)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	Alphabet Site Process Event
 }
