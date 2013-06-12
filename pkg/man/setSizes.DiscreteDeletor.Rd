%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setSizes.DiscreteDeletor}
\alias{setSizes.DiscreteDeletor}
\alias{DiscreteDeletor.setSizes}
\alias{setSizes,DiscreteDeletor-method}

 \title{Set the sizes of the proposed deletions}

 \description{
	Set the sizes of the proposed deletions.

	The provided numeric vector is rounded.
 }

 \usage{
\method{setSizes}{DiscreteDeletor}(this, value, ...)
}

 \arguments{
 	\item{this}{A DiscreteDeletor object.}
 	\item{value}{A numeric vector.}
 	\item{...}{Not used.}
 }

 \value{
 	A vector of integers (invisible).
 }

 \examples{
	# create a DiscreteDeletor object
	d<-DiscreteDeletor(rate=1)
	# set deletion sizes    
	setSizes(d,c(1,2,3))
	# get deletion sizes
	getSizes(d)
	# set/get sizes via virtual field
	d$sizes<-1:10
	d$sizes
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{DiscreteDeletor}}.
 }
