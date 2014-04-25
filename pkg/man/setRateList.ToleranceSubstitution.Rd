%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setRateList.ToleranceSubstitution}
\alias{setRateList.ToleranceSubstitution}
\alias{ToleranceSubstitution.setRateList}
\alias{setRateList,ToleranceSubstitution-method}

 \title{Setting the rates for a ToleranceSubstitution object}

 \description{
	Setting the rates for a ToleranceSubstitution object.

	This method set the rates in the \emph{unscaled} Q  matrix based on the provided list containing even names
	and the associated rates. The rate must be specified for every event!
 }

 \usage{
\method{setRateList}{ToleranceSubstitution}(this, value, ...)
}

 \arguments{
 	\item{this}{A ToleranceSubstitution object.}
	\item{value}{A list with the events names and the associated rates.}
 	\item{...}{Not used.}
 }

 \value{
 	The ToleranceSubstitution object (invisible).
 }

 \examples{
	# create a ToleranceSubstitution object
	# provide an Alphabet object and the rates
	p<-ToleranceSubstitution(alphabet=BinaryAlphabet(), rate.list=list("1->0"=1,"0->1"=3))
	# get the event rates from the unscaled Q matrix
	getRateList(p)
	# get rates from the unscaled rate matrix via virtual field
	p$rateList
	# set rates in the unscaled rate matrix
	setRateList(p, list("0->1"=1,"1->0"=1))
	p$rateList
	# set rates in the unscaled rate matrix via virtual field
	p$rateList<-list("0->1"=3,"1->0"=1);
	# check the contenst of the associated QMatrix object
	summary(p$QMatrix)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{ToleranceSubstitution}}.
 }
