%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getBigRate.Sequence}
\alias{getBigRate.Sequence}
\alias{Sequence.getBigRate}
\alias{getBigRate,Sequence-method}

 \title{Get the sum of all active event rates from a Sequence object}

 \description{
	Get the sum of all active event rates from a Sequence object.
	The sum of active event rates depends on all Site object states and on the attached Process objects.
	It basically returns the last element of the cumulative site rates vector.
 }

 \usage{
\method{getBigRate}{Sequence}(this, ...)
}

 \arguments{
 	\item{this}{A Sequence object.}
 	\item{...}{Not used.}
 }

 \value{
 	A numeric vector of length one.
 }

 \examples{
	# create a nucleotide sequence attach a process
	s<-NucleotideSequence(length=5);
	s$processes<-list(list(JC69()))
	# get the sum of active event rates
	getBigRate(s)	# returns NA because site states are undefined
	# set site states
	s$states<-c("A","T")
	# get big rate via virtual field
	s$bigRate
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Sequence}}.
 }
