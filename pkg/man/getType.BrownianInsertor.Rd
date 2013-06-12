%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getType.BrownianInsertor}
\alias{getType.BrownianInsertor}
\alias{BrownianInsertor.getType}
\alias{getType,BrownianInsertor-method}

 \title{Get the type of the BrownianInsertor process}

 \description{
	Get the type of the BrownianInsertor process.

	If type is \code{discrete}, than the process inherits from \code{DiscreteInsertor}.
	If the type is \code{continuous}, then the object inherits from \code{ContinuousInsertor}.
 }

 \usage{
\method{getType}{BrownianInsertor}(this, ...)
}

 \arguments{
 	\item{this}{A BrownianInsertor object.}
 	\item{...}{Not used.}
 }

 \value{
 	A character vector of length one.
 }

 \examples{
	p<-BrownianInsertor(type="discrete")
	# get type
	getType(p)
	# get upstream classes
	class(p)
	p<-BrownianInsertor(type="continuous")
	# get type via virtual field
	p$type
	# get upstream classes
	class(p)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{BrownianInsertor}}.
 }
