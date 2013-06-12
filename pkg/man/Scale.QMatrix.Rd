%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{Scale.QMatrix}
\alias{Scale.QMatrix}
\alias{QMatrix.Scale}
\alias{Scale,QMatrix-method}

 \title{Scale the scaled rate matrix stored in a QMatrix object by the provided factor}

 \description{
	Scale the scaled rate matrix stored in a QMatrix object by the provided factor.

	This methods sets the scaled rate matrix to \code{unscaled_matrix * constant}.
 }

 \usage{
\method{Scale}{QMatrix}(this, constant=NA, ...)
}

 \arguments{
 	\item{this}{A QMatrix object.}
	\item{constant}{The scaling factor (a numeric vector of length one).}
 	\item{...}{Not used.}
 }

 \value{
 	The QMatrix object (invisible).
 }

 \examples{
	# create a QMatrix object
	# , provide Alphabet object and rates
	m<-QMatrix(name="Susie Q", alphabet=BinaryAlphabet(), rate.list=list("1->0"=2,"0->1"=3))
	# get object summary
	summary(m)
	# perform scaling
	Scale(m, 1/0.666)
	# get object summary
	summary(m)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{QMatrix}}.
 }
