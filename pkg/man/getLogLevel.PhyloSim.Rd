%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getLogLevel.PhyloSim}
\alias{getLogLevel.PhyloSim}
\alias{PhyloSim.getLogLevel}
\alias{getLogLevel,PhyloSim-method}

 \title{Get the log level from a PhyloSim object}

 \description{
	Get the log level from a PhyloSim object.
 }

 \usage{
\method{getLogLevel}{PhyloSim}(this, ...)
}

 \arguments{
 	\item{this}{A PhyloSim object.}
 	\item{...}{Not used.}
 }

 \value{
 	The log level as an integer vector of length one.
 }

 \examples{
	# Create a PhyloSim object
	sim<-PhyloSim();
	# get/set log level
	getLogLevel(sim)
	setLogLevel(sim,0)
	# set/get log level via virtual field
	sim$logLevel<- -1
	sim$logLevel
	# clean up
	unlink(sim$logFile)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	setLogLevel PhyloSim
 }
