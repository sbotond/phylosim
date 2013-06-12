%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{summary.Alphabet}
\alias{summary.Alphabet}
\alias{Alphabet.summary}
\alias{summary,Alphabet-method}

 \title{Summarize the properties of an object}

 \description{
	Summarize the properties of an object.
 }

 \usage{
\method{summary}{Alphabet}(object, ...)
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
	a<-NucleotideAlphabet()
	# get a summary
	summary(a)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Alphabet}}.
 }
