%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{is.Sequence}
\alias{is.Sequence.default}
\alias{is.Sequence}

 \title{Check whether an object inherits from the Sequence class}

 \description{
	Check whether an object inherits from the Sequence class.
 }

 \usage{
\method{is.Sequence}{default}(this, ...)
}

 \arguments{
 	\item{this}{An object.}
 	\item{...}{Not used.}
 }

 \value{
 	TRUE or FALSE
 }

 \examples{
	# create some objects
	seq<-Sequence(length=10)
	a<-Alphabet()
	# check if they inherit from Sequence
	is.Sequence(seq)
	is.Sequence(a)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Sequence}}.
 }

