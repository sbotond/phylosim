%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getLogFile.PhyloSim}
\alias{getLogFile.PhyloSim}
\alias{PhyloSim.getLogFile}
\alias{getLogFile,PhyloSim-method}

 \title{Get the name of the file used for logging}

 \description{
	Get the name of the file used for logging.
 }

 \usage{
\method{getLogFile}{PhyloSim}(this, ...)
}

 \arguments{
 	\item{this}{A PhyloSim object.}
 	\item{...}{Not used.}
 }

 \value{
 	A character vector of length one.
 }

 \examples{
	# Create a PhyloSim object
	sim<-PhyloSim();
	# get the name of the log file
	getLogFile(sim)
	# modify log file name
	setLogFile(sim,"OldLog.txt")
	# get/set log file name via virtual field
	sim$logFile
	sim$logFile<-"NewLog"
	sim$logFile
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PhyloSim}}.
 }
