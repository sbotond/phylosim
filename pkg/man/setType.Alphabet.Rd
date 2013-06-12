%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setType.Alphabet}
\alias{setType.Alphabet}
\alias{Alphabet.setType}
\alias{setType,Alphabet-method}

 \title{Set Alphabet object type}

 \description{
	Set Alphabet object type.
 }

 \usage{
\method{setType}{Alphabet}(this, new_type, ...)
}

 \arguments{
 	\item{this}{An Alphabet object.}
	\item{new_type}{A character vector of length one.}
 	\item{...}{Not used.}
 }

 \value{
 	The new type (invisible).
 }

 \examples{
	# create an alphabet object
	a<-Alphabet(symbols=c(1,2,3))
	# set a new type
	setType(a,"MyAlphabet")
	a$type
	# set type via virtual field
	a$type<-"MorphChars"
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Alphabet}}.
 }
