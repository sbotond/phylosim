%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{hasUndefinedRate.ToleranceSubstitution}
\alias{hasUndefinedRate.ToleranceSubstitution}
\alias{ToleranceSubstitution.hasUndefinedRate}
\alias{hasUndefinedRate,ToleranceSubstitution-method}

 \title{Check if a ToleranceSubstitution object has undefined rates}

 \description{
	Check if a ToleranceSubstitution object has undefined rates.
 }

 \usage{
\method{hasUndefinedRate}{ToleranceSubstitution}(this, ...)
}

 \arguments{
 	\item{this}{A ToleranceSubstitution object.}
 	\item{...}{Not used.}
 }

 \value{
 	TRUE or FALSE.
 }

 \examples{
	# create a ToleranceSubstitution object
	p<-ToleranceSubstitution(alphabet=BinaryAlphabet())
	# check if it has undefined rates
	hasUndefinedRate(p)	# TRUE
	# set the missing rates
	p$rateList<-list("0->1"=1,"1->0"=2)
	# check for undefined rates again
	hasUndefinedRate(p)	# FALSE
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{ToleranceSubstitution}}.
 }
