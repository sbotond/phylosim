%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{is.ToleranceSubstitution}
\alias{is.ToleranceSubstitution.default}
\alias{is.ToleranceSubstitution}

 \title{Check if an object is an instance of the ToleranceSubstitution class}

 \description{
       Check if an object is an instance of the ToleranceSubstitution class.
 }

 \usage{
\method{is.ToleranceSubstitution}{default}(this, ...)
}

 \arguments{
       \item{this}{An object.}
       \item{...}{Not used.}
 }

 \value{
       TRUE or FALSE.
 }

 \examples{
	# create some objects
	p<-ToleranceSubstitution()
	pp<-Process()
	# chek if they inherit from ToleranceSubstitution
	is.ToleranceSubstitution(p)
	is.ToleranceSubstitution(pp)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
       For more information see \code{\link{ToleranceSubstitution}}.
 }

