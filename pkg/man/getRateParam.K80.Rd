%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getRateParam.K80}
\alias{getRateParam.K80}
\alias{K80.getRateParam}
\alias{getRateParam,K80-method}

 \title{Get the value of a rate parameter}

 \description{
	Get the value of a rate parameter.

	 The rate parameters are: Alpha, Beta.
 }

 \usage{
\method{getRateParam}{K80}(this, name, ...)
}

 \arguments{
 	\item{this}{A K80 object.}
	\item{name}{The name of the rate parameter.}
 	\item{...}{Not used.}
 }

 \value{
 	A numeric vector of length one.
 }

 \examples{
	# construct a K80 object
	p<-K80();
	# set/get a rate parameter
	setRateParam(p,"Alpha",4)
	getRateParam(p,"Beta")
	# get object summary
	summary(p)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{K80}}.
 }
