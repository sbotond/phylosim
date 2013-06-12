%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getTableId.CodonAlphabet}
\alias{getTableId.CodonAlphabet}
\alias{CodonAlphabet.getTableId}
\alias{getTableId,CodonAlphabet-method}

 \title{Get the genetic code id}

 \description{
	Get the genetic code id.
 }

 \usage{
\method{getTableId}{CodonAlphabet}(this, ...)
}

 \arguments{
 	\item{this}{A CodonAlphabet object.}
 	\item{...}{Not used.}
 }

 \value{
 	A numeric vector of length one.
 }

 \examples{
	# create CodonAlphabet object
	a<-CodonAlphabet()
	# get genetic code id
	getTableId(a)
	# get genetic code id via virtual field
	a$tableId
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{CodonAlphabet}}.
 }
