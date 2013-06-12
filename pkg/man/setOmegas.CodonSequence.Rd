%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setOmegas.CodonSequence}
\alias{setOmegas.CodonSequence}
\alias{CodonSequence.setOmegas}
\alias{setOmegas,CodonSequence-method}
 \alias{setOmegas.Sequence}

 \title{Set the omegas for a collection of sites}

 \description{
	Set the omegas for a collection of sites.
 }

 \usage{
\method{setOmegas}{CodonSequence}(this, process, value, index, ...)
}

 \arguments{
 	\item{this}{A CodonSequence object.}
	\item{process}{A process object inheriting from GY94.}
	\item{value}{A vector containing the new values of the site-process specific parameter, recycled if shorter than the index vector.}
	\item{index}{A vector of positions. It is set to 1:seq$length if omitted.}
 	\item{...}{Not used.}
 }

 \value{
 	A numeric vector.
 }

 \examples{
	# create a GY94 process
	p<-GY94()
	# create a CodonSequence object,
	# attach a process p
	s<-CodonSequence(length=20,processes=list(list(p)))
	# set omega values in range 1:5
	setOmegas(s,p,c(0.5,1,1.5),1:5)
	# get omega values from siutes 1,2,3,10, and 20
	getOmegas(s,p,c(1:3,10,20))
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{CodonSequence}}.
 }
