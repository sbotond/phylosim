%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getKappa.GY94}
\alias{getKappa.GY94}
\alias{GY94.getKappa}
\alias{getKappa,GY94-method}

 \title{Get the transition/transversion rate ratio}

 \description{
	Get the transition/transversion rate ratio.
 }

 \usage{
\method{getKappa}{GY94}(this, ...)
}

 \arguments{
 	\item{this}{A GY94 object.}
 	\item{...}{Not used.}
 }

 \value{
 	A numeric vector of length one.
 }

 \examples{
	# create a GY94 object
	p<-GY94()
	# set/get kappa
	setKappa(p,2)
	getKappa(p)
	# set/get kappa via virtual field
	p$kappa<-3
	p$kappa
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{GY94}}.
 }
