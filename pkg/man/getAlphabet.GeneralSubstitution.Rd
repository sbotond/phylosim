%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getAlphabet.GeneralSubstitution}
\alias{getAlphabet.GeneralSubstitution}
\alias{GeneralSubstitution.getAlphabet}
\alias{getAlphabet,GeneralSubstitution-method}

 \title{Get the Alphabet object aggregated by a GeneralSubstitution object}

 \description{
	Get the Alphabet object aggregated by a GeneralSubstitution object.

	This method also sets the alphabet for the associated QMatrix object, which will set all rates to NA.
 }

 \usage{
\method{getAlphabet}{GeneralSubstitution}(this, ...)
}

 \arguments{
 	\item{this}{A GeneralSubstitution object.}
 	\item{...}{Not used.}
 }

 \value{
 	An Alphabet object.
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
