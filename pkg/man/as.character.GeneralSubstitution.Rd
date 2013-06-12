%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{as.character.GeneralSubstitution}
\alias{as.character.GeneralSubstitution}
\alias{GeneralSubstitution.as.character}
\alias{as.character,GeneralSubstitution-method}

 \title{Return the character representation of a GeneralSubstitution object}

 \description{
	Return the character representation of a GeneralSubstitution object.
	The character representation is the object id as returned by the
	\code{getId.Process} method defined in the parent class.
 }

 \usage{
\method{as.character}{GeneralSubstitution}(x, ...)
}

 \arguments{
 	\item{x}{A GeneralSubstitution object.}
 	\item{...}{Not used.}
 }

 \value{
 	A character vector of length one.
 }

 \examples{
	# create a GeneralSubstitution object
	p<-GeneralSubstitution(name="MySubst")
	# get character representation
	as.character(p)
	# the same implicitly
	p
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{GeneralSubstitution}}.
 }
