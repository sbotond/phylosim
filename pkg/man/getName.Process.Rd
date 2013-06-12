%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getName.Process}
\alias{getName.Process}
\alias{Process.getName}
\alias{getName,Process-method}

 \title{Get the name of a Process object}

 \description{
	Get the name of a Process object.
 }

 \usage{
\method{getName}{Process}(this, ...)
}

 \arguments{
 	\item{this}{A Process object.}
 	\item{...}{Not used.}
 }

 \value{
 	A charcter vector of length one.
 }

 \examples{
	# create a Process object
	p<-Process()
	# get object name
	getName(p)
	# get name via virtual field
	p$name
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Process}}.
 }
