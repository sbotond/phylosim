%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getRateParamList.K81}
\alias{getRateParamList.K81}
\alias{K81.getRateParamList}
\alias{getRateParamList,K81-method}

 \title{Get the rate parameters}

 \description{
	Get the rate parameters.

	The rate parameters are: Alpha, Beta, Gamma.
 }

 \usage{
\method{getRateParamList}{K81}(this, ...)
}

 \arguments{
 	\item{this}{A K81 object.}
 	\item{...}{Not used.}
 }

 \value{
 	The list of rate parameters.
 }

 \examples{
	# create K81 object
	p<-K81()
	# set/get rate parameters
	setRateParamList(p,list(
		"Alpha"=1,
		"Beta"=0.5,
		"Gamma"=2
        ))
	getRateParamList(p)
	# set/get rate parameters via virtual field
	p$rateParamList<-list(
		"Alpha"=1,
		"Beta"=3,
		"Gamma"=2
        )
	p$rateParamList
	# get object summary
	summary(p)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{K81}}.
 }
