%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setAlphabets.Sequence}
\alias{setAlphabets.Sequence}
\alias{Sequence.setAlphabets}
\alias{setAlphabets,Sequence-method}

 \title{Assotiate Alphabet objects to a set of Site objects aggregated by a Sequence object}

 \description{
	Assotiate Alphabet objects to a set of Site objects aggregated by a Sequence object.
 }

 \usage{
\method{setAlphabets}{Sequence}(this, value, index, ...)
}

 \arguments{
 	\item{this}{A Sequence object.}
 	\item{value}{A list of Alphabet objects, recycled if shorter than the index vector.}
 	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
 	\item{...}{Not used.}
 }

 \value{
 	The Sequence object (invisible).
 }

 \examples{
	# create a Sequence object
	s<-Sequence(length=10)
	# set the alphabets for range 1:5
	setAlphabets(s,list(NucleotideAlphabet(),BinaryAlphabet()),1:5)
	# set the alphabets for range 6:10
	setAlphabets(s,list(AminoAcidAlphabet()),6:10)
	# get the list of attached Alphabet objects
	getAlphabets(s)
	# get Alphabets from a range
	getAlphabets(s,c(2:3,5))
	# set alphabets via virtual field
	s$alphabets<-list(BinaryAlphabet(),NucleotideAlphabet())
	# get alphabets via virtual field
	s$alphabets
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Sequence}}.
 }
