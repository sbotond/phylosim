%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{hasSiteSpecificParameter.Process}
\alias{hasSiteSpecificParameter.Process}
\alias{Process.hasSiteSpecificParameter}
\alias{hasSiteSpecificParameter,Process-method}

 \title{Check if a Process object has the site-process specific parameter with the given id}

 \description{
	Check if a Process object has the site-process specific parameter with the given id.
 }

 \usage{
\method{hasSiteSpecificParameter}{Process}(this, id, ...)
}

 \arguments{
 	\item{this}{A Process object.}
 	\item{id}{The identifier of the site-process specific parameter of interest.}
 	\item{...}{Not used.}
 }

 \value{
 	TRUE or FALSE.
 }

 \examples{
	# create a process object
	p<-Process()
	# check whether it has the "rate.multiplier" site-process specific paramter
	hasSiteSpecificParameter(p,"rate.multiplier");	# TRUE
	# check whether it has the "omega" site-process specific paramter
	hasSiteSpecificParameter(p,"omega");	# FALSE
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Process}}.
 }
