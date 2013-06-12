%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getRateParam.TN93}
\alias{getRateParam.TN93}
\alias{TN93.getRateParam}
\alias{getRateParam,TN93-method}

 \title{Get the value of a rate parameter}

 \description{
	Get the value of a rate parameter.

	 The rate parameters are: Alpha1, Alpha2, Beta.
 }

 \usage{
\method{getRateParam}{TN93}(this, name, ...)
}

 \arguments{
 	\item{this}{A TN93 object.}
	\item{name}{The name of the rate parameter.}
 	\item{...}{Not used.}
 }

 \value{
 	A numeric vector of length one.
 }

 \examples{
	# construct a TN93 object
	p<-TN93();
	# set/get a rate parameter
	setRateParam(p,"Beta",4)
	getRateParam(p,"Beta")
	# get object summary
	summary(p)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{TN93}}.
 }
