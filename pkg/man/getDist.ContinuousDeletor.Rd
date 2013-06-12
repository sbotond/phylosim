%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getDist.ContinuousDeletor}
\alias{getDist.ContinuousDeletor}
\alias{ContinuousDeletor.getDist}
\alias{getDist,ContinuousDeletor-method}

 \title{Get the length sampling expression}

 \description{
	Get the length sampling expression.

	The length sampling expression can be any valid R expression returning
	a numeric vector of length one. The value returned by the expression will be
	rounded.
 }

 \usage{
\method{getDist}{ContinuousDeletor}(this, ...)
}

 \arguments{
 	\item{this}{A ContinuousDeletor object.}
 	\item{...}{Not used.}
 }

 \value{
 	An R expression object.
 }

 \examples{
	# create object
	o<-ContinuousDeletor(rate=1)
	# set/get length sampling expression
	setDist(o, expression(rnorm(1,mean=3, sd=2)))
	getDist(o)
	# set/get length sampling expression via virtual field
	o$dist<-expression(rnorm(1,mean=6,sd=3))
	o$dist
	# set maxLength
	o$maxLength<-10
	# propose a length
	proposeLength(o)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{ContinuousDeletor}}.
 }
