%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getId.PhyloSim}
\alias{getId.PhyloSim}
\alias{PhyloSim.getId}
\alias{getId,PhyloSim-method}

 \title{Get the unique identifier of a PhyloSim object}

 \description{
	Get the unique identifier of a PhyloSim object.
	The unique identifier is the concatenation of the class, the object name as returned by getName() and the object hash
       as returned by hashCode().
 }

 \usage{
\method{getId}{PhyloSim}(this, ...)
}

 \arguments{
 	\item{this}{A PhyloSim object.}
 	\item{...}{Not used.}
 }

 \value{
 	A character vector of length one.
 }

 \examples{
	# create a PhyloSim object
	o<-PhyloSim(name="MySim");
	# get id
	getId(o);
	# get id via virtual field
	o$id;
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PhyloSim}}.
 }
