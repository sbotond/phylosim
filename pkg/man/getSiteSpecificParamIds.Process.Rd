%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getSiteSpecificParamIds.Process}
\alias{getSiteSpecificParamIds.Process}
\alias{Process.getSiteSpecificParamIds}
\alias{getSiteSpecificParamIds,Process-method}

 \title{Get the site specific paramter identifiers from a Process object}

 \description{
	Get the site specific paramter identifiers from a Process object.
 }

 \usage{
\method{getSiteSpecificParamIds}{Process}(this, ...)
}

 \arguments{
 	\item{this}{A Process object.}
 	\item{...}{Not used.}
 }

 \value{
 	A charcter vector.
 }

 \examples{
	# create process object
	p<-Process()
	# get site specific parameter identifiers
	getSiteSpecificParamIds(p)
	# via virtual field
	p$siteSpecificParamIds

 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Process}}.
 }
