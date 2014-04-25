%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{as.character.ToleranceSubstitution}
\alias{as.character.ToleranceSubstitution}
\alias{ToleranceSubstitution.as.character}
\alias{as.character,ToleranceSubstitution-method}

 \title{Return the character representation of a ToleranceSubstitution object}

 \description{
	Return the character representation of a ToleranceSubstitution object.
	The character representation is the object id as returned by the
	\code{getId.Process} method defined in the parent class.
 }

 \usage{
\method{as.character}{ToleranceSubstitution}(x, ...)
}

 \arguments{
 	\item{x}{A ToleranceSubstitution object.}
 	\item{...}{Not used.}
 }

 \value{
 	A character vector of length one.
 }

 \examples{
	# create a ToleranceSubstitution object
	p<-ToleranceSubstitution(name="MySubst")
	# get character representation
	as.character(p)
	# the same implicitly
	p
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{ToleranceSubstitution}}.
 }
