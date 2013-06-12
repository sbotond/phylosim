%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getEquDist.GeneralSubstitution}
\alias{getEquDist.GeneralSubstitution}
\alias{GeneralSubstitution.getEquDist}
\alias{getEquDist,GeneralSubstitution-method}

 \title{Get the equilibrium distribution from a GeneralSubstitution object}

 \description{
	Get the equilibrium distribution from a GeneralSubstitution object.

 }

 \usage{
\method{getEquDist}{GeneralSubstitution}(this, dummy=NA, ...)
}

 \arguments{
 	\item{this}{A GeneralSubstitution object.}
	\item{dummy}{Not used.}
 	\item{...}{Not used.}
 }

 \value{
 	The new equlibrium distribution (invisible).
 }

 \examples{
	# create a GeneralSubstitution object
	# provide an Alphabet object and the rates
	p<-GeneralSubstitution(
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
 	For more information see \code{\link{GeneralSubstitution}}.
 }
