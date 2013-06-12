%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getKappa.F84}
\alias{getKappa.F84}
\alias{F84.getKappa}
\alias{getKappa,F84-method}

 \title{Get the transition transversion rate ratio}

 \description{
	Get the transition transversion rate ratio.
 }

 \usage{
\method{getKappa}{F84}(this, ...)
}

 \arguments{
 	\item{this}{A F84 object.}
 	\item{...}{Not used.}
 }

 \value{
 	A numeric vector of length one.
 }

 \examples{
       # construct a F84 object
       p<-F84();
	# set/get Kappa
	setKappa(p,2)
	getKappa(p)
	# set/get Kappa via virtual field
	p$kappa<-4
	p$kappa
	# get object summary
	summary(p)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{F84}}.
 }
