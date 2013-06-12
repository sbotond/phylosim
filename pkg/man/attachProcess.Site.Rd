%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{attachProcess.Site}
\alias{attachProcess.Site}
\alias{Site.attachProcess}
\alias{attachProcess,Site-method}

 \title{Attach a Process object to a Site object}

 \description{
	Attach a Process object to a Site object.

	The Alphabet objects associated with the Site and Process objects must have the same symbol set, or at least one
	of them should inherit from the class AnyAlphabet.

	During the attachment, the site-process specific parameter templates are copied from the Process object and
	stored in the Site object.
	The Process objects are marked as write protected if the attachment was succesful.

 }

 \usage{
\method{attachProcess}{Site}(this, process, ...)
}

 \arguments{
 	\item{this}{A Site object.}
 	\item{process}{A Process object.}
 	\item{...}{Not used.}
 }

 \value{
	The Site object (invisible).
 }

 \examples{
	# create a Site object and the associated NucleotideAlphabet object
	s<-Site(alphabet=NucleotideAlphabet())
	# create a K80 substitution process
	p<-K80()
	# attach p to s
	attachProcess(s,p)
	# get the list of attached processes
	s$processes
	# check write protection for p
	p$writeProtected
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	Site Process detachProcess setProcesses getProcesses isAttached
 }
