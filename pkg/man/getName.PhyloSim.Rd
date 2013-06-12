%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getName.PhyloSim}
\alias{getName.PhyloSim}
\alias{PhyloSim.getName}
\alias{getName,PhyloSim-method}

 \title{Get the name of a PhyloSim object}

 \description{
	Get the name of a PhyloSim object.
 }

 \usage{
\method{getName}{PhyloSim}(this, ...)
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
	o<-PhyloSim();
	# set/get name
	setName(o,"MySim");
	getName(o,"MySim");
	# set/get name via virtual field
	o$name<-"George";
	o$name
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PhyloSim}}.
 }
