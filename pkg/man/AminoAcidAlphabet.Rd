%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{AminoAcidAlphabet}
\docType{class}
\alias{AminoAcidAlphabet}


 \title{The AminoAcidAlphabet class}

 \description{
	Class of Alphabet objects representing amino acids, using the
	one-letter IUPAC amino acid codes as symbol set:
	\preformatted{
	IUPAC code	Amino acid

	A		Alanine
	C		Cysteine
	D		Aspartic Acid
	E		Glutamic Acid
	F		Phenylalanine
	G		Glycine
	H		Histidine
	I		Isoleucine
	K		Lysine
	L		Leucine
	M		Methionine
	N		Asparagine
	P		Proline
	Q		Glutamine
	R		Arginine
	S		Serine
	T		Threonine
	V		Valine
	W		Tryptophan
	Y		Tyrosine
}

	Package:   \cr
\bold{Class AminoAcidAlphabet}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link{PSRoot}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{\link{Alphabet}}\cr
\code{~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~+--}\code{AminoAcidAlphabet}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{AminoAcidAlphabet}\cr
extends \link{Alphabet}\cr


 }

 \usage{
AminoAcidAlphabet(...)
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
	a<-AminoAcidAlphabet();
	# get object summary
	summary(a)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	Alphabet
 }
