%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{summary.UNREST}
\alias{summary.UNREST}
\alias{UNREST.summary}
\alias{summary,UNREST-method}

 \title{Summarize the properties of an object}

 \description{
       Summarize the properties of an object.
 }

 \usage{
\method{summary}{UNREST}(object, ...)
}

 \arguments{
       \item{object}{An object}
       \item{...}{Not used.}
 }

 \value{
  Returns a PSRootSummary object.
 }

 \examples{
       # create an object
       p<-UNREST(rate.list=list(
		"T->C"=1, "T->A"=2, "T->G"=3, "C->T"=4, "C->A"=1,
		"C->G"=2, "A->T"=3, "A->C"=4, "A->G"=1, "G->T"=2,
		"G->C"=3, "G->A"=4
	))
       # get a summary
       summary(p)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
       For more information see \code{\link{UNREST}}.
 }
