%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setEquDist.ToleranceSubstitution}
\alias{setEquDist.ToleranceSubstitution}
\alias{ToleranceSubstitution.setEquDist}
\alias{setEquDist,ToleranceSubstitution-method}

 \title{Set the equilibrium distribution for a ToleranceSubstitution object}

 \description{
	Set the equilibrium distribution for a ToleranceSubstitution object.

	In the case the equlibrium distribution cannot be guessed from the rate matrix one should provide
	a valid equilibrium distribution. The equilibrium distribution must be compatible with the rate matrix.
	The provided numeric vector will be resacled in the case the sum of the elemnts is not one.
 }

 \usage{
\method{setEquDist}{ToleranceSubstitution}(this, value, force=FALSE, silent=FALSE, ...)
}

 \arguments{
 	\item{this}{A ToleranceSubstitution object.}
	\item{value}{A numeric vector containing the equlibrium symbol frequencies. The order of the frequencies must be the same as in the symbol vector of the attached Alphabet object.}
	\item{force}{Do not check compatibility with thr rate matrix.}
	\item{silent}{Do not print out warnings.}
 	\item{...}{Not used.}
 }

 \value{
 	The new equlibrium distribution (invisible).
 }

 \examples{
	# create a ToleranceSubstitution object
	# provide an Alphabet object and the rates
	p<-ToleranceSubstitution(
                           alphabet=BinaryAlphabet(),
                           rate.list=list("1->0"=1,"0->1"=1)
                           )
	# get equlibrium distribution
	getEquDist(p)
	# get equilibrium distribution via virtual field
	p$equDist
	# re-set the equilibrium distribution
	dist<-p$equDist * 3
	dist
	setEquDist(p,dist)
	p$equDist
	# re-set equilibrium distribution via virtual field
	p$equDist<-p$equDist * 2
	p$equDist
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{ToleranceSubstitution}}.
 }
