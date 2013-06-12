%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{copySubSequence.Sequence}
\alias{copySubSequence.Sequence}
\alias{Sequence.copySubSequence}
\alias{copySubSequence,Sequence-method}

 \title{Copy a collection of Site objects aggregated by a Sequence object into a new Sequence object}

 \description{
	Copy a collection of Site objects aggregated by a Sequence object into a new Sequence object.
 }

 \usage{
\method{copySubSequence}{Sequence}(this, index, process=NA, ...)
}

 \arguments{
 	\item{this}{A Sequence object.}
	\item{index}{An index vector specifying a collection of sites to be copied. It is set to 1:seq$length if omitted.}
 	\item{process}{The Process object performing the copy (optional).}
 	\item{...}{Not used.}
 }

 \value{
 	A new Sequence object.
 }

 \examples{
	# create a nucleotide sequence
	s<-NucleotideSequence(string="ATATATATATATATATA")
	# copy sites in the range 3:8 in a new object
	s2<-copySubSequence(s,3:8)
	s2
	# copy sites 1,3 and 5 from s2
	s3<-copySubSequence(s2,c(1,3,5))
	s3
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Sequence}}.
 }
