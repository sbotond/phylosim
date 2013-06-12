%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getRateList.GeneralSubstitution}
\alias{getRateList.GeneralSubstitution}
\alias{GeneralSubstitution.getRateList}
\alias{getRateList,GeneralSubstitution-method}

 \title{Get a list of events and their unscaled rates from a GeneralSubstitution object}

 \description{
	Get a list of events and their unscaled rates from a GeneralSubstitution object.

	This method returns the list of event rates from the \emph{unscaled} Q matrix (as returbed bvy the \code{getEventRate} method).
	The returned list contains the rates associated with the corresponding event names.
 }

 \usage{
\method{getRateList}{GeneralSubstitution}(this, ...)
}

 \arguments{
 	\item{this}{A GeneralSubstitution object.}
 	\item{...}{Not used.}
 }

 \value{
 	A list of event rates.
 }

 \examples{
	# create a GeneralSubstitution object
	# provide an Alphabet object and the rates
	p<-GeneralSubstitution(alphabet=BinaryAlphabet(), rate.list=list("1->0"=1,"0->1"=3))
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
 	For more information see \code{\link{GeneralSubstitution}}.
 }
