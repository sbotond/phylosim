%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setLogFile.PhyloSim}
\alias{setLogFile.PhyloSim}
\alias{PhyloSim.setLogFile}
\alias{setLogFile,PhyloSim-method}

 \title{Set the name of the file used for logging}

 \description{
	Set the name of the file used for logging.
 }

 \usage{
\method{setLogFile}{PhyloSim}(this, value, ...)
}

 \arguments{
 	\item{this}{A PhyloSim object.}
 	\item{value}{The name of the file used for logging.}
 	\item{...}{Not used.}
 }

 \value{
 	The new logfile.
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
