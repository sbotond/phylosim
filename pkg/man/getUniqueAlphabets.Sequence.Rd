%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getUniqueAlphabets.Sequence}
\alias{getUniqueAlphabets.Sequence}
\alias{Sequence.getUniqueAlphabets}
\alias{getUniqueAlphabets,Sequence-method}

 \title{Get the list of unique Alphabet objects associated to Site objects aggaregated by a Sequence object}

 \description{
	Get the list of unique Alphabet objects associated to Site objects aggaregated by a Sequence object.
	The returned list contains unique instances of the Alphabet class. The symbol sets are not compared, so
	two instances of the same class are considered to be different.
 }

 \usage{
\method{getUniqueAlphabets}{Sequence}(this, ...)
}

 \arguments{
 	\item{this}{A Sequence object.}
 	\item{...}{Not used.}
 }

 \value{
 	A list of Alphabet objects.
 }

 \examples{
	# create a Sequence object with some Alphabet objects attached
	s<-Sequence(
               alphabets=list(NucleotideAlphabet(),
               BinaryAlphabet(),
               NucleotideAlphabet()),
               length=10
               )
	# get the list of attached alphabets
	s$alphabets
	# get the unique list of attahced Alphabet objects
	getUniqueAlphabets(s)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Sequence}}.
 }
