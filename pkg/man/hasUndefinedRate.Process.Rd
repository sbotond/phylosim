%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{hasUndefinedRate.Process}
\alias{hasUndefinedRate.Process}
\alias{Process.hasUndefinedRate}
\alias{hasUndefinedRate,Process-method}

 \title{Check if the Process object has undefined rate parameters}

 \description{
	Check if the Process object has undefined rate parameters.

	For the instances of the Process class this method always returns FALSE.
	Descendant classes should implement more meaningful methods.
 }

 \usage{
\method{hasUndefinedRate}{Process}(this, ...)
}

 \arguments{
 	\item{this}{A Process object.}
 	\item{...}{Not used.}
 }

 \value{
 	FALSE
 }

 \examples{
	# create object
	p<-Process()
	# check if has undefined rates
	hasUndefinedRate(p)	# return FALSE

 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Process}}.
 }
