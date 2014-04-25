%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getAlphabet.ToleranceSubstitution}
\alias{getAlphabet.ToleranceSubstitution}
\alias{ToleranceSubstitution.getAlphabet}
\alias{getAlphabet,ToleranceSubstitution-method}

 \title{Get the Alphabet object aggregated by a ToleranceSubstitution object}

 \description{
	Get the Alphabet object aggregated by a ToleranceSubstitution object.

	This method also sets the alphabet for the associated QMatrix object, which will set all rates to NA.
 }

 \usage{
\method{getAlphabet}{ToleranceSubstitution}(this, ...)
}

 \arguments{
 	\item{this}{A ToleranceSubstitution object.}
 	\item{...}{Not used.}
 }

 \value{
 	An Alphabet object.
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
