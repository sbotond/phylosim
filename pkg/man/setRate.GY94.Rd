%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setRate.GY94}
\alias{setRate.GY94}
\alias{GY94.setRate}
\alias{setRate,GY94-method}

 \title{Set an unscaled rate for an event from a GY94 object}

 \description{
       Set an unscaled rate for an event from a GY94 object.
     
	See \code{\link{setRate.GeneralSubstitution}}.
 }

 \usage{
\method{setRate}{GY94}(this, name=NA, value, from=NA, to=NA, ...)
}

 \arguments{
       \item{this}{A GeneralSubstitution object.}
       \item{name}{The name of the event.}
       \item{from}{The initial state.}
       \item{value}{The new value of the rate.}
       \item{to}{Target state.}
       \item{...}{Not used.}
 }

 \value{
       A Numeric vector of length one.
 }


 \author{Botond Sipos, Gregory Jordan}

 \seealso{
       For more information see \code{\link{GY94}}.
 }
