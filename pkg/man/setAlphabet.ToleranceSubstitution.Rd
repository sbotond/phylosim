%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setAlphabet.ToleranceSubstitution}
\alias{setAlphabet.ToleranceSubstitution}
\alias{ToleranceSubstitution.setAlphabet}
\alias{setAlphabet,ToleranceSubstitution-method}

 \title{Set the Alphabet object aggregated by a ToleranceSubstitution object}

 \description{
	Set the Alphabet object aggregated by a ToleranceSubstitution object.

	This method also sets the alphabet for the associated QMatrix object, which will set all rates to NA.
	This method will also re-initialize the equlibrium distribution by setting all frequencies to NA.
 }

 \usage{
\method{setAlphabet}{ToleranceSubstitution}(this, value, ...)
}

 \arguments{
 	\item{this}{A ToleranceSubstitution object.}
	\item{value}{An Alphabet object.}
 	\item{...}{Not used.}
 }

 \value{
 	The Alphabet object.
 }

 \examples{
	# create a ToleranceSubstitution object with an attached BinaryAlphabet object
	p<-ToleranceSubstitution(alphabet=BinaryAlphabet())
	# get object summary
	summary(p)
	# get alphabet
	getAlphabet(p)
	# get alphabet via virtual field
	p$alphabet
	# set a new alphabet
	setAlphabet(p,NucleotideAlphabet())
	summary(p)
	# set alphabet via virtual field
	p$alphabet<-BinaryAlphabet()
	p$alphabet
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{ToleranceSubstitution}}.
 }
