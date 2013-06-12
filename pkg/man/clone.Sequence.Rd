%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{clone.Sequence}
\alias{clone.Sequence}
\alias{Sequence.clone}
\alias{clone,Sequence-method}

 \title{Clone a Sequence object}

 \description{
	Clone a Sequence object.

	The cloning of Sequence objects involves the cloning of all aggregated Site objects. Because of that the
	cloning of long sequences is quite expensive.
	The cloned Site objects have the orginal Site objects as ancestral.
	The new Sequence objects has the original object as ancestral.

 }

 \usage{
\method{clone}{Sequence}(this, ...)
}

 \arguments{
 	\item{this}{A Sequence object.}
 	\item{...}{Not used.}
 }

 \value{
 	A Sequence object.
 }

 \examples{
	# cretate a nucleotide sequence
	s<-NucleotideSequence(string="ATG")
	# clone the sequence
	cs<-clone(s)
	# get some properties
	equals(s,s)
	equals(s,cs)
	cs$ancestral
	cs$sites[[1]]$ancestral
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	Sequence clone.Object
 }
