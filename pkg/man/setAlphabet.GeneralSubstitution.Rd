%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setAlphabet.GeneralSubstitution}
\alias{setAlphabet.GeneralSubstitution}
\alias{GeneralSubstitution.setAlphabet}
\alias{setAlphabet,GeneralSubstitution-method}

 \title{Set the Alphabet object aggregated by a GeneralSubstitution object}

 \description{
	Set the Alphabet object aggregated by a GeneralSubstitution object.

	This method also sets the alphabet for the associated QMatrix object, which will set all rates to NA.
	This method will also re-initialize the equlibrium distribution by setting all frequencies to NA.
 }

 \usage{
\method{setAlphabet}{GeneralSubstitution}(this, value, ...)
}

 \arguments{
 	\item{this}{A GeneralSubstitution object.}
	\item{value}{An Alphabet object.}
 	\item{...}{Not used.}
 }

 \value{
 	The Alphabet object.
 }

 \examples{
	# create a GeneralSubstitution object with an attached BinaryAlphabet object
	p<-GeneralSubstitution(alphabet=BinaryAlphabet())
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
 	For more information see \code{\link{GeneralSubstitution}}.
 }
