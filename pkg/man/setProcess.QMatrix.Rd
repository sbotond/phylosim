%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setProcess.QMatrix}
\alias{setProcess.QMatrix}
\alias{QMatrix.setProcess}
\alias{setProcess,QMatrix-method}

 \title{Assotiate a process object with a QMatrix object}

 \description{
	Assotiate a process object with a QMatrix object.
 }

 \usage{
\method{setProcess}{QMatrix}(this, value, ...)
}

 \arguments{
 	\item{this}{A QMatrix object.}
	\item{value}{A Process object.}
 	\item{...}{Not used.}
 }

 \value{
 	The Process object.
 }

 \examples{
	# Create a GeneralSubstitution object
	p<-GeneralSubstitution(alphabet=BinaryAlphabet())
	p
	# get the associated QMatrix object from p
	m<-p$qMatrix
	summary(m)
	# get the associated process from m
	m$process
	# clone p
	pp<-clone(p)
	# assotiate m with pp
	pp$qMatrix<-m
	# assotiate pp with m
	m$process<-pp
	m$process
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{QMatrix}}.
 }
