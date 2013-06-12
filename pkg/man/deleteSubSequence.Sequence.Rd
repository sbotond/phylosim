%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{deleteSubSequence.Sequence}
\alias{deleteSubSequence.Sequence}
\alias{Sequence.deleteSubSequence}
\alias{deleteSubSequence,Sequence-method}

 \title{Delete a collection of sites aggregated by a Sequence object}

 \description{
	Delete a collection of sites aggregated by a Sequence object.
 }

 \usage{
\method{deleteSubSequence}{Sequence}(this, index, ...)
}

 \arguments{
 	\item{this}{A Sequence object.}
	\item{index}{An index vector specifying a collection of sites to be deleted. It is set to 1:seq$length if omitted.}
 	\item{...}{Not used.}
 }

 \value{
 	Invisible TRUE or errror.
 }

 \examples{
	# create a nucleotide sequence
	s<-NucleotideSequence(string="ATATATATATATATAT")
	# delete sites 2, 4 and 6
	deleteSubSequence(s,c(2,4,6))
	s
	# delete sites in the range 3:6
	deleteSubSequence(s,3:6)
	s
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Sequence}}.
 }
