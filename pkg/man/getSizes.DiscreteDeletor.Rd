%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getSizes.DiscreteDeletor}
\alias{getSizes.DiscreteDeletor}
\alias{DiscreteDeletor.getSizes}
\alias{getSizes,DiscreteDeletor-method}

 \title{Get the sizes of the proposed deletions}

 \description{
	Get the sizes of the proposed deletions.
 }

 \usage{
\method{getSizes}{DiscreteDeletor}(this, ...)
}

 \arguments{
 	\item{this}{A DiscreteDeletor object.}
 	\item{...}{Not used.}
 }

 \value{
 	A vector of integers.
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
