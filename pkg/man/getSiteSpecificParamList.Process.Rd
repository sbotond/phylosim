%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getSiteSpecificParamList.Process}
\alias{getSiteSpecificParamList.Process}
\alias{Process.getSiteSpecificParamList}
\alias{getSiteSpecificParamList,Process-method}

 \title{Get the list of site specific parameters of a Process object}

 \description{
	Get the list of site specific parameters of a Process object.
	Every site specific parameter is a list storing the name, the identifier and the type of the given parameter.
 }

 \usage{
\method{getSiteSpecificParamList}{Process}(this, ...)
}

 \arguments{
 	\item{this}{A Process object.}
 	\item{...}{Not used.}
 }

 \value{
 	A list of site specific parameters.
 }

 \examples{
	# create a process object
	p<-Process()
	# get the list of site specific parameters
	getSiteSpecificParamList(p)
	# get it via virtual field
	p$siteSpecificParamList
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Process}}.
 }
