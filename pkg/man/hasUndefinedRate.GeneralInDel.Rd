%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{hasUndefinedRate.GeneralInDel}
\alias{hasUndefinedRate.GeneralInDel}
\alias{GeneralInDel.hasUndefinedRate}
\alias{hasUndefinedRate,GeneralInDel-method}

 \title{Check whether the general rate of a GeneralInDel object is undefined}

 \description{
	Check whether the general rate of a GeneralInDel object is undefined.
 }

 \usage{
\method{hasUndefinedRate}{GeneralInDel}(this, ...)
}

 \arguments{
 	\item{this}{A GeneralInDel object.}
 	\item{...}{Not used.}
 }

 \value{
 	TRUE or FALSE.
 }

 \examples{
	# create a GeneralInDel object
	o<-GeneralInDel()
	# check if the general rate is undefined
	hasUndefinedRate(o)
	# set general rate
	o$rate<-1
	# check rate again
	hasUndefinedRate(o)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{GeneralInDel}}.
 }
