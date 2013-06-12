%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{globalConsistencyCheck.PSRoot}
\alias{globalConsistencyCheck.PSRoot}
\alias{PSRoot.globalConsistencyCheck}
\alias{globalConsistencyCheck,PSRoot-method}

 \title{Check the consistency of all objects inheriting form PSRoot in the current environment}

 \description{
	Check the consistency of all objects inheriting form PSRoot in the current environment.

	This method searches for objects which inherit from PSRoot and calls \code{checkConsistency()} for all of them,
	which can take a lots of time. Prints the results of the checks as text.
 }

 \usage{
\method{globalConsistencyCheck}{PSRoot}(...)
}

 \arguments{
 	\item{...}{Not used.}
 }

 \value{
 	Returns invisible TRUE if all checks are successful.
 }

 \examples{
	# create some objects
	a<-NucleotideAlphabet()
	s<-Site()
	p<-Process()
	# ask for a global consistency check
	PSRoot$globalConsistencyCheck();
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PSRoot}}.
 }
