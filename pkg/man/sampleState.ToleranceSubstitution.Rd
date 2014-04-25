%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{sampleState.ToleranceSubstitution}
\alias{sampleState.ToleranceSubstitution}
\alias{ToleranceSubstitution.sampleState}
\alias{sampleState,ToleranceSubstitution-method}

 \title{Sample a state from the equlibrium distribution of a ToleranceSubstitution object}

 \description{
	Sample a state from the equlibrium distribution of a ToleranceSubstitution object.
 }

 \usage{
\method{sampleState}{ToleranceSubstitution}(this, ...)
}

 \arguments{
 	\item{this}{A ToleranceSubstitution object.}
 	\item{...}{Not used.}
 }

 \value{
 	A character vector of length one.
 }

 \examples{
	# create a ToleranceSubstitution object
	# provide an Alphabet object and the rates
	p<-ToleranceSubstitution(alphabet=BinaryAlphabet(), rate.list=list("1->0"=1,"0->1"=1))
	# get equlibrium distribution
	getEquDist(p)
	# get equilibrium distribution via virtual field
	p$equDist
	# sample from equilibrium distribution
	sampleState(p)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{ToleranceSubstitution}}.
 }
