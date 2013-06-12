%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getId.Process}
\alias{getId.Process}
\alias{Process.getId}
\alias{getId,Process-method}

 \title{Get the unique identifier of a Process object}

 \description{
	Get the unique identifier of a Process object.
	The unique identifier is the concatenation of the class, the object name as returned by getName() and the object hash
	as returned by hashCode().
 }

 \usage{
\method{getId}{Process}(this, ...)
}

 \arguments{
 	\item{this}{A Process object.}
 	\item{...}{Not used.}
 }

 \value{
 	A character vector of length one.
 }

 \examples{
	# create a Process object
	p<-Process()
	# get unique id
	getId(p)
	# get unique id via virtual field
	p$id

 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Process}}.
 }
