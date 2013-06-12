%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setLogLevel.PhyloSim}
\alias{setLogLevel.PhyloSim}
\alias{PhyloSim.setLogLevel}
\alias{setLogLevel,PhyloSim-method}

 \title{Set the log level for a given PhyloSim object}

 \description{
	Set the log level for a given PhyloSim object.

	No logging is performed if the log level is negative. If the log level is zero, the messages passed to
	the \code{Log} method will be writen in the log file. If the log level is positive, the messages passed to
	the \code{Debug} method are saved as well.

	The default log level is -1. The specified file will be truncated in the case it already exists.
 }

 \usage{
\method{setLogLevel}{PhyloSim}(this, value, ...)
}

 \arguments{
 	\item{this}{A PhyloSim object.}
	\item{value}{The new log level as an integer.}
 	\item{...}{Not used.}
 }

 \value{
 	The new level as an integer vector of length one.
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
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	getLogLevel PhyloSim
 }
