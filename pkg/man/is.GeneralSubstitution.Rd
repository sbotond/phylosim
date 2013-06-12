%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{is.GeneralSubstitution}
\alias{is.GeneralSubstitution.default}
\alias{is.GeneralSubstitution}

 \title{Check if an object is an instance of the GeneralSubstitution class}

 \description{
       Check if an object is an instance of the GeneralSubstitution class.
 }

 \usage{
\method{is.GeneralSubstitution}{default}(this, ...)
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
	p<-GeneralSubstitution()
	pp<-Process()
	# chek if they inherit from GeneralSubstitution
	is.GeneralSubstitution(p)
	is.GeneralSubstitution(pp)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
       For more information see \code{\link{GeneralSubstitution}}.
 }

