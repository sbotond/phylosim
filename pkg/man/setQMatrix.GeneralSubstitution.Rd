%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setQMatrix.GeneralSubstitution}
\alias{setQMatrix.GeneralSubstitution}
\alias{GeneralSubstitution.setQMatrix}
\alias{setQMatrix,GeneralSubstitution-method}

 \title{Set the QMatrix object aggregated by a GeneralSubstitution object}

 \description{
	Set the QMatrix object aggregated by a GeneralSubstitution object.

	This method is mostly used internally.
 }

 \usage{
\method{setQMatrix}{GeneralSubstitution}(this, value, ...)
}

 \arguments{
 	\item{this}{A GeneralSubstitution object.}
	\item{value}{A QMatrix object.}
 	\item{...}{Not used.}
 }

 \value{
 	The QMatrix object.
 }

 \examples{
	# create a GeneralSubstitution object
	# provide an Alphabet object and the rates
	p<-GeneralSubstitution(alphabet=BinaryAlphabet(), rate.list=list("1->0"=1,"0->1"=1))
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
 	For more information see \code{\link{GeneralSubstitution}}.
 }
