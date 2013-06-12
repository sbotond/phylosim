%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getRateParam.HKY}
\alias{getRateParam.HKY}
\alias{HKY.getRateParam}
\alias{getRateParam,HKY-method}

 \title{Get the value of a rate parameter}

 \description{
	Get the value of a rate parameter.

	 The rate parameters are: Alpha, Beta.
 }

 \usage{
\method{getRateParam}{HKY}(this, name, ...)
}

 \arguments{
 	\item{this}{An HKY object.}
	\item{name}{The name of the rate parameter.}
 	\item{...}{Not used.}
 }

 \value{
 	The list of rate parameters.
 }

 \examples{
	# construct HKY object
	p<-HKY();
	# set/get a rate parameter
	setRateParam(p,"Alpha",4)
	getRateParam(p,"Beta")
	# get object summary
	summary(p)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{HKY}}.
 }
