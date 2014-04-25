%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setQMatrix.ToleranceSubstitution}
\alias{setQMatrix.ToleranceSubstitution}
\alias{ToleranceSubstitution.setQMatrix}
\alias{setQMatrix,ToleranceSubstitution-method}

 \title{Set the QMatrix object aggregated by a ToleranceSubstitution object}

 \description{
	Set the QMatrix object aggregated by a ToleranceSubstitution object.

	This method is mostly used internally.
 }

 \usage{
\method{setQMatrix}{ToleranceSubstitution}(this, value, ...)
}

 \arguments{
 	\item{this}{A ToleranceSubstitution object.}
	\item{value}{A QMatrix object.}
 	\item{...}{Not used.}
 }

 \value{
 	The QMatrix object.
 }

 \examples{
	# create a ToleranceSubstitution object
	# provide an Alphabet object and the rates
	p<-ToleranceSubstitution(alphabet=BinaryAlphabet(), rate.list=list("1->0"=1,"0->1"=1))
	# get the QMatrix object
	getQMatrix(p)
	# get the QMatrix object via virtual field
	q<-p$qMatrix
	# tweak with the QMatrix
	setRate(q,"0->1",2)
	# set a new QMatrix for p
	setQMatrix(p,q)
	summary(p)
	# set new QMatrix via virtual field
	setRate(q,"1->0",2)
	p$qMatrix<-q
	summary(p)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{ToleranceSubstitution}}.
 }
