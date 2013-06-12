%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getAncestral.Sequence}
\alias{getAncestral.Sequence}
\alias{Sequence.getAncestral}
\alias{getAncestral,Sequence-method}

 \title{Get the ancestral object of a Sequence object}

 \description{
	Get the ancestral object of a Sequence object.
 }

 \usage{
\method{getAncestral}{Sequence}(this, ...)
}

 \arguments{
 	\item{this}{A Sequence object.}
 	\item{...}{Not used.}
 }

 \value{
 	A Sequence object or a Process object.
 }

 \examples{
	# create a sequence object
	s<-Sequence(length=4)
	# get ancestral object
	getAncestral(s)	# newly created sequences have the "Root insertion process" as ancestral
	# clone sequence
	cs<-clone(s)
	# get ancestral object id via virtual field
	cs$ancestral$id
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Sequence}}.
 }
