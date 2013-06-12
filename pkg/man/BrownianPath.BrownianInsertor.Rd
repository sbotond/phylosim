%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{BrownianPath.BrownianInsertor}
\alias{BrownianPath.BrownianInsertor}
\alias{BrownianInsertor.BrownianPath}
\alias{BrownianPath,BrownianInsertor-method}

 \title{Generate a Brownian path}

 \description{
	Generate a Brownian path.

	This method generates a Brownian path given the scale parameter a (determining "noisiness")
	and the vector p describing the trends. More useful as a static method.
 }

 \usage{
\method{BrownianPath}{BrownianInsertor}(this, p=NA, a=NA, ...)
}

 \arguments{
	\item{this}{A BrownianInsertor object.}
 	\item{p}{Path parameter (a numeric vector).}
 	\item{a}{Scale paramater (a numeric vector of length one).}
 	\item{...}{Not used.}
 }

 \value{
 	A numeric vector.
 }

 \examples{
	path<-BrownianInsertor$BrownianPath(a=2,p=1:10);
	path
	plot(path)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{BrownianInsertor}}.
 }
