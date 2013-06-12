%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setRateParam.F84}
\alias{setRateParam.F84}
\alias{F84.setRateParam}
\alias{setRateParam,F84-method}

 \title{Set the value of a rate parameter}

 \description{
       Set the value of a rate parameter.

        The rate parameters are: Kappa.
 }

 \usage{
\method{setRateParam}{F84}(this, name, value, ...)
}

 \arguments{
       \item{this}{A F84 object.}
       \item{name}{The name of the rate parameter.}
       \item{value}{A numeric vector of length one.}
       \item{...}{Not used.}
 }

 \value{
       The new value of the rate parameter (invisible).
 }

 \examples{
       # construct a F84 object
       p<-F84();
       # set/get a rate parameter
       setRateParam(p,"Kappa",4)
       getRateParam(p,"Kappa")
       # get object summary
       summary(p)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
       For more information see \code{\link{F84}}.
 }
