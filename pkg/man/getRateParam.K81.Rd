%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getRateParam.K81}
\alias{getRateParam.K81}
\alias{K81.getRateParam}
\alias{getRateParam,K81-method}

 \title{Get the value of a rate parameter}

 \description{
	Get the value of a rate parameter.

	 The rate parameters are: Alpha, Beta, Gamma.
 }

 \usage{
\method{getRateParam}{K81}(this, name, ...)
}

 \arguments{
 	\item{this}{A K81 object.}
	\item{name}{The name of the rate parameter.}
 	\item{...}{Not used.}
 }

 \value{
 	The value of the rate parameter.
 }

 \examples{
	# construct a K81 object
	p<-K81();
	# set/get a rate parameter
	setRateParam(p,"Alpha",4)
	getRateParam(p,"Gamma")
	# get object summary
	summary(p)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{K81}}.
 }
