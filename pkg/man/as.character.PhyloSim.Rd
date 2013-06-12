%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{as.character.PhyloSim}
\alias{as.character.PhyloSim}
\alias{PhyloSim.as.character}
\alias{as.character,PhyloSim-method}

 \title{Return the character representation of a PhyloSim object}

 \description{
	Return the character representation of a PhyloSim object.

	The character representation is the identifier of the PhyloSim object as returned by the \code{getId} method.
 }

 \usage{
\method{as.character}{PhyloSim}(x, ...)
}

 \arguments{
 	\item{x}{A PhyloSim object.}
 	\item{...}{Not used.}
 }

 \value{
 	A character vector of length one.
 }

 \examples{
	# create a PhyloSim object
	o<-PhyloSim(name="MySim");
	# get character representation
	as.character(o)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PhyloSim}}.
 }
