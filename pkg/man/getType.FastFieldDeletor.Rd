%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getType.FastFieldDeletor}
\alias{getType.FastFieldDeletor}
\alias{FastFieldDeletor.getType}
\alias{getType,FastFieldDeletor-method}

 \title{Get the type of a FastFieldDeletor object}

 \description{
	Get the type of a FastFieldDeletor object.
 }

 \usage{
\method{getType}{FastFieldDeletor}(this, ...)
}

 \arguments{
 	\item{this}{A FastFieldDeletor object.}
 	\item{...}{Not used.}
 }

 \value{
 	A character vector of length one.
 }

 \examples{
	# create a FastFieldDeletor, default type (geometric)
	p<-FastFieldDeletor()
	# get type
	getType(p)
	# create a FastFieldDeletor, poisson type
	p<-FastFieldDeletor(type="poisson")
	p$type
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{FastFieldDeletor}}.
 }
