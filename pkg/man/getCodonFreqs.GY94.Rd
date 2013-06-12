%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getCodonFreqs.GY94}
\alias{getCodonFreqs.GY94}
\alias{GY94.getCodonFreqs}
\alias{getCodonFreqs,GY94-method}

 \title{Get codon frequencies}

 \description{
	Get codon frequencies.
 }

 \usage{
\method{getCodonFreqs}{GY94}(this, ...)
}

 \arguments{
 	\item{this}{A GY94 object.}
 	\item{...}{Not used.}
 }

 \value{
 	A matrix containing the codon frequencies.
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
