%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{BinaryAlphabet}
\docType{class}
\alias{BinaryAlphabet}


 \title{The BinaryAlphabet class}

 \description{
	 Class of Alphabet objects with the c("0","1") symbol set.

	Package:   \cr
\bold{Class BinaryAlphabet}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link{PSRoot}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{\link{Alphabet}}\cr
\code{~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~+--}\code{BinaryAlphabet}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{BinaryAlphabet}\cr
extends \link{Alphabet}\cr


 }

 \usage{
BinaryAlphabet(...)
}

 \arguments{
 	\item{...}{Not used.}
	}

 \section{Fields and Methods}{
 	\bold{Methods:}\cr
\emph{No methods defined}.


 \bold{Methods inherited from Alphabet}:\cr
!=, ==, as.character, checkConsistency, getSize, getSymbolLength, getSymbols, getType, getWriteProtected, hasSymbols, is, isEmpty, setSize, setSymbolLength, setSymbols, setType, summary

 \bold{Methods inherited from PSRoot}:\cr
checkConsistency, enableVirtual, getComments, getMethodsList, globalConsistencyCheck, intersect.list, is.na, is, ll, my.all.equal, plot, setComments, setMethodsList, summary, virtualAssignmentForbidden

 \bold{Methods inherited from Object}:\cr
[[, [[<-, $, $<-, as.character, attach, attachLocally, clearCache, clearLookupCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFieldModifier, getFieldModifiers, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save

 }

 \examples{
	# create a binary alphabet
	b<-BinaryAlphabet()
	# get alphabet summary
	summary(b)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	Alphabet
 }
