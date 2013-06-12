%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setRateParamList.HKY}
\alias{setRateParamList.HKY}
\alias{HKY.setRateParamList}
\alias{setRateParamList,HKY-method}

 \title{Set the rate parameters}

 \description{
	Set the rate parameters.

	The rate parameters are: Alpha, Beta.
 }

 \usage{
\method{setRateParamList}{HKY}(this, value, ...)
}

 \arguments{
 	\item{this}{An HKY object.}
	\item{value}{A list containing the rate parameters.}
 	\item{...}{Not used.}
 }

 \value{
 	The list of rate parameters (invisible).
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
