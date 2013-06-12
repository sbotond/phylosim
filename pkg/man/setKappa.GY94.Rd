%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setKappa.GY94}
\alias{setKappa.GY94}
\alias{GY94.setKappa}
\alias{setKappa,GY94-method}

 \title{Set the transition/transversion rate ratio}

 \description{
	Set the transition/transversion rate ratio.
 }

 \usage{
\method{setKappa}{GY94}(this, value, ...)
}

 \arguments{
 	\item{this}{A GY94 object.}
	\item{value}{A numeric vector of length one.}
 	\item{...}{Not used.}
 }

 \value{
 	The new value of kappa.
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
