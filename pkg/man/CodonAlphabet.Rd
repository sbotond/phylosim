%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{CodonAlphabet}
\docType{class}
\alias{CodonAlphabet}


 \title{The CodonAlphabet class}

 \description{
	This class implements codon alphabets and handles the translation of codons
	to their corresponding amino acids. Stop codons are excluded from the symbol set,
	thus the symbol set depends on the genetic code table.
	The genetic code table can be specified through the \code{table.id} constructor parameter.

	The available genetic code tables:
	\preformatted{
	1	Standard
	2	Vertebrate Mitochondrial
	3	Yeast Mitochondrial
	4	Mold, Protozoan, and CoelenterateMitochondrial and Mycoplasma/Spiroplasma
	5	Invertebrate Mitochondrial
	6	Ciliate, Dasycladacean and Hexamita Nuclear
	9	Echinoderm Mitochondrial
	10	Euplotid Nuclear
	11	Bacterial
	12	Alternative Yeast Nuclear
	13	Ascidian Mitochondrial
	14	Flatworm Mitochondrial
	15	Blepharisma Nuclear
	16	Chlorophycean Mitochondrial
	21	Trematode Mitochondrial
	22	Scenedesmus obliquus Mitochondrial
	23	Thraustochytrium Mitochondrial
	}

	Package:   \cr
\bold{Class CodonAlphabet}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link{PSRoot}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{\link{Alphabet}}\cr
\code{~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~+--}\code{CodonAlphabet}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{CodonAlphabet}\cr
extends \link{Alphabet}\cr


 }

 \usage{
CodonAlphabet(table.id=1, ...)
}

 \arguments{
 	\item{table.id}{The identifier of the genetic code table.}
 	\item{...}{Not used.}
	}

 \section{Fields and Methods}{
 	\bold{Methods:}\cr
\tabular{rll}{
 \tab \code{areSynonymous} \tab  -\cr
 \tab \code{checkConsistency} \tab  -\cr
 \tab \code{getTableId} \tab  -\cr
 \tab \code{getTransTable} \tab  -\cr
 \tab \code{is} \tab  -\cr
 \tab \code{isStartCodon} \tab  -\cr
 \tab \code{isStopCodon} \tab  -\cr
 \tab \code{setTableId} \tab  -\cr
 \tab \code{setTransTable} \tab  -\cr
 \tab \code{summary} \tab  -\cr
 \tab \code{translateCodon} \tab  -\cr
}


 \bold{Methods inherited from Alphabet}:\cr
!=, ==, as.character, checkConsistency, getSize, getSymbolLength, getSymbols, getType, getWriteProtected, hasSymbols, is, isEmpty, setSize, setSymbolLength, setSymbols, setType, summary

 \bold{Methods inherited from PSRoot}:\cr
checkConsistency, enableVirtual, getComments, getMethodsList, globalConsistencyCheck, intersect.list, is.na, is, ll, my.all.equal, plot, setComments, setMethodsList, summary, virtualAssignmentForbidden

 \bold{Methods inherited from Object}:\cr
[[, [[<-, $, $<-, as.character, attach, attachLocally, clearCache, clearLookupCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFieldModifier, getFieldModifiers, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save

 }

 \examples{
	# create a CodonAlphabet object
	a<-CodonAlphabet(table.id=2)
	# get object summary
	summary(a)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{CodonAlphabet}}.
 }
