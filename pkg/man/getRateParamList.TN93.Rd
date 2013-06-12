%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getRateParamList.TN93}
\alias{getRateParamList.TN93}
\alias{TN93.getRateParamList}
\alias{getRateParamList,TN93-method}

 \title{Get the rate parameters}

 \description{
	Get the rate parameters.

	The rate parameters are: Alpha1, Alpha2, Beta.
 }

 \usage{
\method{getRateParamList}{TN93}(this, ...)
}

 \arguments{
 	\item{this}{A TN93 object.}
 	\item{...}{Not used.}
 }

 \value{
 	The list of rate parameters.
 }

 \examples{
	# create TN93 object
	p<-TN93()
	# set/get rate parameters
	setRateParamList(p,list(
		"Alpha1"=1,
		"Alpha2"=2,
		"Beta"=0.5
        ))
	getRateParamList(p)
	# set/get rate parameters via virtual field
	p$rateParamList<-list(
		"Alpha1"=1,
		"Alpha2"=1,
		"Beta"=3
        )
	p$rateParamList
	# get object summary
	summary(p)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{TN93}}.
 }
