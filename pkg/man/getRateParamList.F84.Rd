%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getRateParamList.F84}
\alias{getRateParamList.F84}
\alias{F84.getRateParamList}
\alias{getRateParamList,F84-method}

 \title{Get the rate parameters}

 \description{
       Get the rate parameters.

       The rate parameters are: Kappa.
 }

 \usage{
\method{getRateParamList}{F84}(this, ...)
}

 \arguments{
       \item{this}{A F84 object.}
       \item{...}{Not used.}
 }

 \value{
       The list of rate parameters.
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
