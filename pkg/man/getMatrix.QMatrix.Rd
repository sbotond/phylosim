%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getMatrix.QMatrix}
\alias{getMatrix.QMatrix}
\alias{QMatrix.getMatrix}
\alias{getMatrix,QMatrix-method}

 \title{Get the unscaled rate matrix form a QMatrix object}

 \description{
	Get the unscaled rate matrix form a QMatrix object.
 }

 \usage{
\method{getMatrix}{QMatrix}(this, ...)
}

 \arguments{
 	\item{this}{A QMatrix object.}
 	\item{...}{Not used.}
 }

 \value{
 	A matrix object.
 }

 \examples{
       # create a GeneralSubstitution object
       # provide an Alphabet object and the rates
       p<-GeneralSubstitution(alphabet=BinaryAlphabet(), rate.list=list("1->0"=1,"0->1"=3))
	# get the QMatrix object from p
	m<-p$QMatrix
	# get the unscaled rate matrix from m
	m$matrix
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{QMatrix}}.
 }
