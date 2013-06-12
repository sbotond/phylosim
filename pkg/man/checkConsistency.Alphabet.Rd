%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{checkConsistency.Alphabet}
\alias{checkConsistency.Alphabet}
\alias{Alphabet.checkConsistency}
\alias{checkConsistency,Alphabet-method}

 \title{Check object consistency}

 \description{
		Check object consistency.
 }

 \usage{
\method{checkConsistency}{Alphabet}(this, ...)
}

 \arguments{
 	\item{this}{An object.}
 	\item{...}{Not used.}
 }

 \value{
		Returns an invisible TRUE if no inconsistencies found, throws an error otherwise.
 }

 \examples{
		# create an alphabet object
		a<-Alphabet(symbols=c(0,1));
		# check consistency
		print(checkConsistency(a));
		# mess up with the internals
		a$.symbols[1]<-"BAD";
		# NOT run: cosistency check now will throw an error
		\dontrun{ print(checkConsistency(a)); }

 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Alphabet}}.
 }
