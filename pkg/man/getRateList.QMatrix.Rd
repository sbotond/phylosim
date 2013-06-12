%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getRateList.QMatrix}
\alias{getRateList.QMatrix}
\alias{QMatrix.getRateList}
\alias{getRateList,QMatrix-method}

 \title{Get a list of events and their unscaled rates from a QMatrix object}

 \description{
       Get a list of events and their unscaled rates from a QMatrix object.

       This method returns the list of event rates from the \emph{unscaled} rate matrix.
       The returned list contains the rates associated with the corresponding event names.
 }

 \usage{
\method{getRateList}{QMatrix}(this, ...)
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
	# get the QMatrix object from p
	m<-p$QMatrix
       # get the event rates from the unscaled Q matrix
       getRateList(m)
       # get rates from the unscaled rate matrix via virtual field
       m$rateList
       # set rates in the unscaled rate matrix
       setRateList(m, list("0->1"=1,"1->0"=1))
       m$rateList
       # set rates in the unscaled rate matrix via virtual field
       m$rateList<-list("0->1"=3,"1->0"=1);
	m$rateList
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
       For more information see \code{\link{QMatrix}}.
 }
