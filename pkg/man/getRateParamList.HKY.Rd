%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getRateParamList.HKY}
\alias{getRateParamList.HKY}
\alias{HKY.getRateParamList}
\alias{getRateParamList,HKY-method}

 \title{Get the rate parameters}

 \description{
	Get the rate parameters.

	The rate parameters are: Alpha, Beta.
 }

 \usage{
\method{getRateParamList}{HKY}(this, ...)
}

 \arguments{
 	\item{this}{An HKY object.}
 	\item{...}{Not used.}
 }

 \value{
 	The list of rate parameters.
 }

 \examples{
	# create HKY object
	p<-HKY()
	# set/get rate parameters
	setRateParamList(p,list(
		"Alpha"=1,
		"Beta"=0.5
        ))
	getRateParamList(p)
	# set/get rate parameters via virtual field
	p$rateParamList<-list(
		"Alpha"=1,
		"Beta"=3
        )
	p$rateParamList
	# get object summary
	summary(p)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{HKY}}.
 }
