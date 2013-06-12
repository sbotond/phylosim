%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{is.CodonUNREST}
\alias{is.CodonUNREST.default}
\alias{is.CodonUNREST}

 \title{Check whether an object inherits from CodonUNREST}

 \description{
		Check whether an object inherits from CodonUNREST.
 }

 \usage{
\method{is.CodonUNREST}{default}(this, ...)
}

 \arguments{
		\item{this}{An object.}
		\item{...}{Not used.}

 }

 \value{
	TRUE of FALSE.
 }

 \examples{
	# create some objects
	p<-CodonUNREST()
	pp<-GTR()
	# check if they inherit from CodonUNREST
	is.CodonUNREST(p)
	is.CodonUNREST(pp)
 }

 \author{Botond Sipos, Gregory Jordan}

