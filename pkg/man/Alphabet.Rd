%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{Alphabet}
\docType{class}
\alias{Alphabet}


 \title{The Alphabet class}

 \description{

		Class representing an alphabet (a set of symbols).
		Package:   \cr
\bold{Class Alphabet}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link{PSRoot}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{Alphabet}\cr

\bold{Directly known subclasses:}\cr
\link{AminoAcidAlphabet}, \link{AnyAlphabet}, \link{BinaryAlphabet}, \link{CodonAlphabet}, \link{NucleotideAlphabet}\cr

public static class \bold{Alphabet}\cr
extends \link{PSRoot}\cr



 }

 \usage{
Alphabet(symbols=NA, type="Generic", ...)
}

 \arguments{
 	\item{symbols}{A character vector containing the symbols for the alphabet. All elements must have the
	same length and no duplicates are allowed. No element of the vector may contain the dash symbol, which is reserved for gaps.}
 	\item{type}{An identifier for the Alphabet object.}
	\item{...}{Not used.}
	}

 \section{Fields and Methods}{
		\bold{Methods:}\cr
\tabular{rll}{
 \tab \code{!=} \tab  -\cr
 \tab \code{==} \tab  -\cr
 \tab \code{as.character} \tab  -\cr
 \tab \code{checkConsistency} \tab  -\cr
 \tab \code{getSize} \tab  -\cr
 \tab \code{getSymbolLength} \tab  -\cr
 \tab \code{getSymbols} \tab  -\cr
 \tab \code{getType} \tab  -\cr
 \tab \code{getWriteProtected} \tab  -\cr
 \tab \code{hasSymbols} \tab  -\cr
 \tab \code{is} \tab  -\cr
 \tab \code{isEmpty} \tab  -\cr
 \tab \code{setSize} \tab  -\cr
 \tab \code{setSymbolLength} \tab  -\cr
 \tab \code{setSymbols} \tab  -\cr
 \tab \code{setType} \tab  -\cr
 \tab \code{setWriteProtected} \tab  -\cr
 \tab \code{summary} \tab  -\cr
}


 \bold{Methods inherited from PSRoot}:\cr
checkConsistency, enableVirtual, getComments, getMethodsList, globalConsistencyCheck, intersect.list, is.na, is, ll, my.all.equal, plot, setComments, setMethodsList, summary, virtualAssignmentForbidden

 \bold{Methods inherited from Object}:\cr
[[, [[<-, $, $<-, as.character, attach, attachLocally, clearCache, clearLookupCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFieldModifier, getFieldModifiers, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save

 }

 \examples{
		# create an alphabet object
		a<-Alphabet(type="Binary",symbols=c("0","1"));
		# print summary
		summary(a);
		# change the identifier
		a$type<-"Nucleotide";
		# change the symbol set
		a$symbols<-c("A","T","G","C");
		# print summary again
		summary(a);
		# clone the alphabet object
		b<-clone(a);
		# test the equality of the symbol sets
		a == b;
 }

 \author{Botond Sipos, Gregory Jordan}


 \seealso{
 	Site Process Event BinaryAlphabet NucleotideAlphabet AminoAcidAlphabet
 }
