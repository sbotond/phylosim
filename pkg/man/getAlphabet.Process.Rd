%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getAlphabet.Process}
\alias{getAlphabet.Process}
\alias{Process.getAlphabet}
\alias{getAlphabet,Process-method}

 \title{Get the Alphabet object associated with a given Process object}

 \description{
	Get the Alphabet object associated with a given Process object.
 }

 \usage{
\method{getAlphabet}{Process}(this, ...)
}

 \arguments{
 	\item{this}{A Process object.}
 	\item{...}{Not used.}
 }

 \value{
 	An Alphabet object.
 }

 \examples{
	# create a process object
	p<-Process(alphabet=NucleotideAlphabet())
	# get associated Alphabet object
	getAlphabet(p)
	# via virtual field
	p$alphabet
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Process}}.
 }
