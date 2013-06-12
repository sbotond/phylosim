%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{Debug.PhyloSim}
\alias{Debug.PhyloSim}
\alias{PhyloSim.Debug}
\alias{Debug,PhyloSim-method}

 \title{Save a debug message in the PhyloSim log file}

 \description{
	Save a debug message in the PhyloSim log file.

	The debug message is written to the log file only if the log level is non-negative. You can use this method for logging
	debug messages in the case you write classes for PhyloSim.
 }

 \usage{
\method{Debug}{PhyloSim}(this, message, ...)
}

 \arguments{
 	\item{this}{A PhyloSim object.}
 	\item{message}{A character vector of length one.}
 	\item{...}{Not used.}
 }

 \value{
 	The message (invisible).
 }

 \examples{
	# create a PhyloSim object,
	# with logLevel set to zero
	sim<-PhyloSim(log.level=0);
	# log a debug message
	Debug(sim,"Some useful detail...");
	# close log connection
	close(sim$.log.connection)
	# print out the log file
	cat(paste(scan(file=sim$LogFile,what=character(),sep="\n"),collapse="\n"));cat("\n");
	# clean up
	unlink(sim$logFile)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PhyloSim}}.
 }
