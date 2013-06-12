%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{omegaHist.CodonSequence}
\alias{omegaHist.CodonSequence}
\alias{CodonSequence.omegaHist}
\alias{omegaHist,CodonSequence-method}

 \title{Plot a histogram of omega values from a range}

 \description{
	Plot a histogram of omega values from a range.
 }

 \usage{
\method{omegaHist}{CodonSequence}(this, process, breaks, index, ...)
}

 \arguments{
 	\item{this}{A CodonSequence object.}
	\item{process}{A process object inheriting from GY94.}
	\item{breaks}{\code{breaks} parameter for \code{hist()}.}
	\item{index}{A vector of positions.}
 	\item{...}{Not used.}
 }

 \value{
 	The CodonSequence object (invisible).
 }

 \examples{
	# create a GY94 process
	p<-GY94()
	# create a CodonSequence object,
	# attach a process p
	s<-CodonSequence(length=20,processes=list(list(p)))
       # set omega values through omegaVarM2.CodonSequence
       omegaVarM2(s,p,p0=0.5,p1=0.2,omega=1.5)
       # get a histogram of omega values from the range 1:15
       omegaHist(s,p,breaks=10,1:15)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{CodonSequence}}.
 }
