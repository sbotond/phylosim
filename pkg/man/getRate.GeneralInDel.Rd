%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getRate.GeneralInDel}
\alias{getRate.GeneralInDel}
\alias{GeneralInDel.getRate}
\alias{getRate,GeneralInDel-method}

 \title{Get the general rate}

 \description{
	Get the general rate.
 }

 \usage{
\method{getRate}{GeneralInDel}(this, ...)
}

 \arguments{
 	\item{this}{A GeneralInDel object.}
 	\item{...}{Not used.}
 }

 \value{
 	A numeric vector of length one.
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
