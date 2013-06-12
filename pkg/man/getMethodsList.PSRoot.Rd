%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getMethodsList.PSRoot}
\alias{getMethodsList.PSRoot}
\alias{PSRoot.getMethodsList}
\alias{getMethodsList,PSRoot-method}

 \title{Get the list of applicable methods for an object}

 \description{
	Get the list of applicable methods for an object.
 }

 \usage{
\method{getMethodsList}{PSRoot}(this, ...)
}

 \arguments{
 	\item{this}{A PSRoot object.}
 	\item{...}{Not used.}
 }

 \value{
 	The list of applicable methods.
 }

 \examples{
	# create an object
	o<-PSRoot()
	# get the applicable methods
	getMethodsList(o)
	# get methods via virtual field
	o$methodsList
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PSRoot}}.
 }
