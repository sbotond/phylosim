%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setRateParamList.F84}
\alias{setRateParamList.F84}
\alias{F84.setRateParamList}
\alias{setRateParamList,F84-method}

 \title{Set the rate parameters}

 \description{
       Set the rate parameters.

       The rate parameters are: Kappa.
 }

 \usage{
\method{setRateParamList}{F84}(this, value, ...)
}

 \arguments{
       \item{this}{A F84 object.}
       \item{value}{A list containing the rate parameters.}
       \item{...}{Not used.}
 }

 \value{
       The list of rate parameters (invisible).
 }

 \examples{
       # create F84 object
       p<-F84()
       # set/get rate parameters
       setRateParamList(p,list("Kappa"=3))
       getRateParamList(p)
       # set/get rate parameters via virtual field
       p$rateParamList<-list("Kappa"=2.5)
       p$rateParamList
       # get object summary
       summary(p)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
       For more information see \code{\link{F84}}.
 }
