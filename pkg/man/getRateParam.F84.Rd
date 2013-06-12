%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getRateParam.F84}
\alias{getRateParam.F84}
\alias{F84.getRateParam}
\alias{getRateParam,F84-method}

 \title{Get the value of a rate parameter}

 \description{
       Get the value of a rate parameter.

        The rate parameters are: Kappa.
 }

 \usage{
\method{getRateParam}{F84}(this, name, ...)
}

 \arguments{
       \item{this}{A F84 object.}
       \item{name}{The name of the rate parameter.}
       \item{...}{Not used.}
 }

 \value{
       A numeric vector of length one.
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
