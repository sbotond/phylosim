%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getProcess.QMatrix}
\alias{getProcess.QMatrix}
\alias{QMatrix.getProcess}
\alias{getProcess,QMatrix-method}

 \title{Get the process object associated with a QMatrix object}

 \description{
	Get the process object associated with a QMatrix object.
 }

 \usage{
\method{getProcess}{QMatrix}(this, ...)
}

 \arguments{
 	\item{this}{A QMatrix object.}
 	\item{...}{Not used.}
 }

 \value{
 	A process object, most likely one which inherits from GeneralSubstitution.
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
