%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{is.GY94}
\alias{is.GY94.default}
\alias{is.GY94}

 \title{Check whether an object inherits from GY94}

 \description{
		Check whether an object inherits from GY94.
 }

 \usage{
\method{is.GY94}{default}(this, ...)
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
	pp<-GY94()
	# check if they inherit from CodonUNREST
	is.GY94(p)
	is.GY94(pp)
 }

 \author{Botond Sipos, Gregory Jordan}

