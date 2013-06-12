%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getProcesses.Site}
\alias{getProcesses.Site}
\alias{Site.getProcesses}
\alias{getProcesses,Site-method}

 \title{Get the list of Process objects attached to a Site object}

 \description{
	Get the list of Process objects attached to a Site object.
 }

 \usage{
\method{getProcesses}{Site}(this, ...)
}

 \arguments{
 	\item{this}{A Site object.}
 	\item{...}{Not used.}
 }

 \value{
 	A list of Process objects.
 }

 \examples{
	# create a Site object with some processes attached
	s<-Site(alphabet=NucleotideAlphabet(),processes=list(K80(),JC69()))
	# get list of attached Process objects
	getProcesses(s)
	# get list of attached Process objects via virtual field
	s$processes
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Site}}.
 }
