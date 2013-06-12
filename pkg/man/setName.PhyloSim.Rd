%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setName.PhyloSim}
\alias{setName.PhyloSim}
\alias{PhyloSim.setName}
\alias{setName,PhyloSim-method}

 \title{Set the name of a PhyloSim object}

 \description{
	Set the name of a PhyloSim object.
 }

 \usage{
\method{setName}{PhyloSim}(this, new.name, ...)
}

 \arguments{
 	\item{this}{A PhyloSim object.}
	\item{new.name}{A character vector of length one.}
 	\item{...}{Not used.}
 }

 \value{
 	The new name.
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
