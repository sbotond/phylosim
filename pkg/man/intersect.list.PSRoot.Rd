%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{intersect.list.PSRoot}
\alias{intersect.list.PSRoot}
\alias{PSRoot.intersect.list}
\alias{intersect.list,PSRoot-method}

 \title{Utility method returning the intersection of two lists}

 \description{
	Utility method returning the intersection of two lists.
	Duplicated elements are not considered.
 }

 \usage{
\method{intersect.list}{PSRoot}(this, one, two, ...)
}

 \arguments{
 	\item{this}{A PSRoot object.}
	\item{one}{A list of objects.}
	\item{two}{A list of objects.}
 	\item{...}{Not used.}
 }

 \value{
 	The list containing the intersection.
 }

 \examples{
	# create some lists
	a<-list(1,2,3);
	b<-c(a,list("a","b","c"))
	# get the intersection of a and b
	PSRoot$intersect.list(a,b)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PSRoot}}.
 }
