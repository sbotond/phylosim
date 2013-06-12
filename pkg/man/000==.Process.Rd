%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{==.Process}
\alias{==.Process}
\alias{Process.==}
\alias{==,Process-method}
 \alias{!=.Process}

 \title{Check whether the two supplied Process objects are identical}

 \description{
	Check whether the two supplied Process objects are identical.
 }

 \usage{
\method{==}{Process}(e1, e2, ...)
}

 \arguments{
 	\item{e1}{A Process object.}
 	\item{e2}{A Process object.}
 	\item{...}{Not used.}
 }

 \value{
	TRUE or FALSE;
 }

 \examples{
	# create some Process objects
	p1<-Process()
	p2<-clone(p1)
	# check object equality
	p1 == p1
	p1 == p2
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Process}}.
 }
