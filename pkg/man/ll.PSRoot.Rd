%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{ll.PSRoot}
\alias{ll.PSRoot}
\alias{PSRoot.ll}
\alias{ll,PSRoot-method}

 \title{Display detailed information about the virtual fields and methods defined for a given object}

 \description{
	Display detailed information about the virtual fields and methods defined for a given object.
	The method prints the class of the object, all the parent classes,
	and the virtual fields and methods defined in the immediate class.

	This method provides a "quick and minimal" documentation for PhyloSim classes.
 }

 \usage{
\method{ll}{PSRoot}(this, quiet=FALSE, ...)
}

 \arguments{
 	\item{this}{A PSRoot object.}
 	\item{quiet}{Do not print out methods list.}
 	\item{...}{Not used.}
 }

 \value{
 	Text.
 }

 \examples{
	# create a Site object
	s<-Site()
	ll(s)
	# get information about the Process class
	ll(Process())
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PSRoot}}.
 }
