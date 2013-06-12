%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{hasUndefinedRate.GeneralSubstitution}
\alias{hasUndefinedRate.GeneralSubstitution}
\alias{GeneralSubstitution.hasUndefinedRate}
\alias{hasUndefinedRate,GeneralSubstitution-method}

 \title{Check if a GeneralSubstitution object has undefined rates}

 \description{
	Check if a GeneralSubstitution object has undefined rates.
 }

 \usage{
\method{hasUndefinedRate}{GeneralSubstitution}(this, ...)
}

 \arguments{
 	\item{this}{A GeneralSubstitution object.}
 	\item{...}{Not used.}
 }

 \value{
 	TRUE or FALSE.
 }

 \examples{
	# create a GeneralSubstitution object
	p<-GeneralSubstitution(alphabet=BinaryAlphabet())
	# check if it has undefined rates
	hasUndefinedRate(p)	# TRUE
	# set the missing rates
	p$rateList<-list("0->1"=1,"1->0"=2)
	# check for undefined rates again
	hasUndefinedRate(p)	# FALSE
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{GeneralSubstitution}}.
 }
