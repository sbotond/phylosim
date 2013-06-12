%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setRate.GeneralInDel}
\alias{setRate.GeneralInDel}
\alias{GeneralInDel.setRate}
\alias{setRate,GeneralInDel-method}

 \title{Set the general rate}

 \description{
	Set the general rate.
 }

 \usage{
\method{setRate}{GeneralInDel}(this, value, ...)
}

 \arguments{
 	\item{this}{A GeneralInDel object.}
	\item{value}{The new general rate (a numeric vector of length one).}
 	\item{...}{Not used.}
 }

 \value{
 	The new general rate.
 }

 \examples{
	# create a GeneralInDel object
	o<-GeneralInDel(rate=0.5)
	# get/set general rate
	getRate(o)
	setRate(o, 1.5)
	# get/set rate via virtual field
	o$rate
	o$rate<-0.3
	o$rate
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{GeneralInDel}}.
 }
