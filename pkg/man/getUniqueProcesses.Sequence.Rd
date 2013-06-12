%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getUniqueProcesses.Sequence}
\alias{getUniqueProcesses.Sequence}
\alias{Sequence.getUniqueProcesses}
\alias{getUniqueProcesses,Sequence-method}

 \title{Get the list of unique Process instances attached to the Site objects aggregated by a Sequence object}

 \description{
	Get the list of unique Process instances attached to the Site objects aggregated by a Sequence object.
 }

 \usage{
\method{getUniqueProcesses}{Sequence}(this, ...)
}

 \arguments{
 	\item{this}{A Sequence object.}
 	\item{...}{Not used.}
 }

 \value{
 	A list of Process objects.
 }

 \examples{
	# create a sequence object and attach processes
	p<-JC69()
	s<-Sequence(
               length=4,
               alphabets=list(NucleotideAlphabet()),
               processes=list(list(p,K80()),list(p))
               )
	# get the unique list of attached Process instances
	getUniqueProcesses(s)
	# via virtual field
	s$uniqueProcesses
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Sequence}}.
 }
