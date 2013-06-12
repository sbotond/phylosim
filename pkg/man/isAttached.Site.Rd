%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{isAttached.Site}
\alias{isAttached.Site}
\alias{Site.isAttached}
\alias{isAttached,Site-method}

 \title{Check whether a Process object is attached to a Site object}

 \description{
	Check whether a Process object is attached to a Site object.
 }

 \usage{
\method{isAttached}{Site}(this, process, ...)
}

 \arguments{
 	\item{this}{A Site object.}
 	\item{process}{A Process object.}
 	\item{...}{Not used.}
 }

 \value{
 	TRUE or FALSE.
 }

 \examples{
	# create a Site object
	s<-Site(alphabet=NucleotideAlphabet())
	# create a Process object
	p<-JC69()
	# check if p is attached to s
	isAttached(s,p)
	# attach p to s
	s$processes<-list(p)
	isAttached(s,p)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	Site Process attachProcess detachProcess getProcesses setProcesses
 }
