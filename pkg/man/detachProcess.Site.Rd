%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{detachProcess.Site}
\alias{detachProcess.Site}
\alias{Site.detachProcess}
\alias{detachProcess,Site-method}

 \title{Site}

 \description{
	Site.
	The site-process specific parameters stored in the Site object and belonging to the detached Process objects will be destroyed.

 }

 \usage{
\method{detachProcess}{Site}(this, process, ...)
}

 \arguments{
 	\item{this}{A Site object}
 	\item{process}{A Process object}
 	\item{...}{Not used.}
 }

 \value{
 	The Site object (invisible).
 }

 \examples{
	# create a Site objects and attach some processes
	s<-Site()
	s$alphabet<-NucleotideAlphabet()
	p1<-JC69(); p2<-K80()
	attachProcess(s,p1)
	attachProcess(s,p2)
	# get the list of attached processes
	s$processes
	# detach p1
	detachProcess(s,p1)
	s$processes
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Site}}.
 }
