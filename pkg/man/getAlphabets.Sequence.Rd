%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getAlphabets.Sequence}
\alias{getAlphabets.Sequence}
\alias{Sequence.getAlphabets}
\alias{getAlphabets,Sequence-method}

 \title{Get the list of the Alphabet objects attached to the Site objects aggregated by a Sequence object}

 \description{
	Get the list of the Alphabet objects attached to the Site objects aggregated by a Sequence object.
 }

 \usage{
\method{getAlphabets}{Sequence}(this, index, ...)
}

 \arguments{
 	\item{this}{A Sequence object.}
 	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
 	\item{...}{Not used.}
 }

 \value{
 	A list of Alphabet objects.
 }

 \examples{
	# create a Sequence object with NucleotideAlphabet
	#and BinaryAlphabet objects attached
	s<-Sequence(alphabets=list(NucleotideAlphabet(),BinaryAlphabet()),length=5)
	setStates(s,c("A","0"))
	# get the list of attached Alphabet objects
	getAlphabets(s)
	# get Alphabets from a range
	getAlphabets(s,c(2:3,5))
	# get alphabets via virtual field
	s$alphabets
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Sequence}}.
 }
