%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getRateParamList.GTR}
\alias{getRateParamList.GTR}
\alias{GTR.getRateParamList}
\alias{getRateParamList,GTR-method}

 \title{Get the rate parameters}

 \description{
	Get the rate parameters.

	 The rate parameters are named as in PAML (see PAML documentation: \url{http://bit.ly/9SQK2f}).
 }

 \usage{
\method{getRateParamList}{GTR}(this, ...)
}

 \arguments{
 	\item{this}{A GTR object.}
 	\item{...}{Not used.}
 }

 \value{
 	A list of rate parameters.
 }

 \examples{
	# create GTR object
	p<-GTR()
	# set/get rate parameters
	setRateParamList(p,list(
                       "a"=1, "b"=2, "c"=3,
                       "d"=1, "e"=2, "f"=3
        ))
	getRateParamList(p)
	# set/get rate parameters via virtual field
	p$rateParamList<-list(
                       "a"=4, "b"=1, "c"=4,
                       "d"=1, "e"=4, "f"=1
        )
	p$rateParamList
	# get object summary
	summary(p)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{GTR}}.
 }
