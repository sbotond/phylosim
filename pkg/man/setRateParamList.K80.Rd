%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setRateParamList.K80}
\alias{setRateParamList.K80}
\alias{K80.setRateParamList}
\alias{setRateParamList,K80-method}

 \title{Set the rate parameters}

 \description{
	Set the rate parameters.

	The rate parameters are: Alpha, Beta.
 }

 \usage{
\method{setRateParamList}{K80}(this, value, ...)
}

 \arguments{
 	\item{this}{A K80 object.}
	\item{value}{A list containing the rate parameters.}
 	\item{...}{Not used.}
 }

 \value{
 	The list of rate parameters (invisible).
 }

 \examples{
	# create K80 object
	p<-K80()
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
 	For more information see \code{\link{K80}}.
 }
