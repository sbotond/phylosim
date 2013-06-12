%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{AnyAlphabet}
\docType{class}
\alias{AnyAlphabet}


 \title{The AnyAlphabet class}

 \description{
	This is a special Alphabet class which matches any alphabet.
	The '=='.Alphabet method always returns TRUE when one of the
	compared objects inherits from AnyAlphabet. This behaviour is
	handy when creating processes that have no alphabet preference
	(like a deletion process).

		Package:   \cr
\bold{Class AnyAlphabet}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link{PSRoot}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{\link{Alphabet}}\cr
\code{~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~+--}\code{AnyAlphabet}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{AnyAlphabet}\cr
extends \link{Alphabet}\cr


 }

 \usage{
AnyAlphabet(...)
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
	# create some alphabet objects
	a<-BinaryAlphabet()
	b<-NucleotideAlphabet()
	any<-AnyAlphabet()
	# compare objects
	a == b
	any == a
	any == b
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	Alphabet
 }
