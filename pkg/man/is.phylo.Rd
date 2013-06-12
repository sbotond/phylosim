%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{is.phylo}
\alias{is.phylo.default}
\alias{is.phylo}

 \title{Check if an object is an instance of the phylo class}

 \description{
	Check if an object is an instance of the phylo class.
	Phylo objects are created by the \pkg{APE} package. This method just return the value of \code{inherits(this,"phylo")}.
 }

 \usage{
\method{is.phylo}{default}(this, ...)
}

 \arguments{
 	\item{this}{An object.}
 	\item{...}{Not used.}
 }

 \value{
 	TRUE or FALSE.
 }

 \examples{
	# load APE
	library(ape);
	# create some objects
	o1<-Object();
	o2<-rcoal(3);
	# check if they are phylo objects
	is.phylo(o1);
	is.phylo(o2);	

 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	The \pkg{ape} package.
 }

