%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setCodonFreqs.GY94}
\alias{setCodonFreqs.GY94}
\alias{GY94.setCodonFreqs}
\alias{setCodonFreqs,GY94-method}

 \title{Get codon frequencies}

 \description{
	Get codon frequencies.
 }

 \usage{
\method{setCodonFreqs}{GY94}(this, value, ...)
}

 \arguments{
 	\item{this}{A GY94 object.}
	\item{value}{A vector of codon frequencies.}
 	\item{...}{Not used.}
 }

 \value{
 	value (invisible)
 }

 \examples{
	# create a GY94 object
	p<-GY94()
	# get codon frequencies
	getCodonFreqs(p)
	p$codonFreqs
	# set codon frequencies
	p$codonFreqs<-rep(c(1,2,3,4),length.out=61)
	p$codonFreqs
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{GY94}}.
 }
