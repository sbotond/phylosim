%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setAncestral.Sequence}
\alias{setAncestral.Sequence}
\alias{Sequence.setAncestral}
\alias{setAncestral,Sequence-method}

 \title{Set the ancestral object of a Sequence object}

 \description{
	Set the ancestral object of a Sequence object.
 }

 \usage{
\method{setAncestral}{Sequence}(this, value, ...)
}

 \arguments{
 	\item{this}{A Sequence object.}
	\item{value}{A Sequence or a Process object.}
 	\item{...}{Not used.}
 }

 \value{
 	The new ancestral object (invisible).
 }

 \examples{
	# create a nucleotide sequence and a process object
	s<-NucleotideSequence(string="AGCT")
	p<-Process(name="MyProcess")
	# set the p as the ancestral of s
	setAncestral(s,p)
	s$ancestral
	# clone s
	cs<-clone(s)
	# set cs as ancestral of s via virtual field
	s$ancestral<-cs
	# get ancestral ids
	s$ancestral$id
	cs$ancestral$id
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Sequence}}.
 }
