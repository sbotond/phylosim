%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{sampleState.GeneralSubstitution}
\alias{sampleState.GeneralSubstitution}
\alias{GeneralSubstitution.sampleState}
\alias{sampleState,GeneralSubstitution-method}

 \title{Sample a state from the equlibrium distribution of a GeneralSubstitution object}

 \description{
	Sample a state from the equlibrium distribution of a GeneralSubstitution object.
 }

 \usage{
\method{sampleState}{GeneralSubstitution}(this, ...)
}

 \arguments{
 	\item{this}{A GeneralSubstitution object.}
 	\item{...}{Not used.}
 }

 \value{
 	A character vector of length one.
 }

 \examples{
	# create a GeneralSubstitution object
	# provide an Alphabet object and the rates
	p<-GeneralSubstitution(alphabet=BinaryAlphabet(), rate.list=list("1->0"=1,"0->1"=1))
	# get equlibrium distribution
	getEquDist(p)
	# get equilibrium distribution via virtual field
	p$equDist
	# sample from equilibrium distribution
	sampleState(p)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{GeneralSubstitution}}.
 }
