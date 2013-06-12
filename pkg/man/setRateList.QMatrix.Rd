%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setRateList.QMatrix}
\alias{setRateList.QMatrix}
\alias{QMatrix.setRateList}
\alias{setRateList,QMatrix-method}

 \title{Setting the unscaled rates stored in a QMatrix object}

 \description{
       Setting the unscaled rates stored in a QMatrix object.
     
       This method set the rates in the \emph{unscaled} Q  matrix based on the provided list containing even names
       and the associated rates. The rate must be specified for every event!
 }

 \usage{
\method{setRateList}{QMatrix}(this, value, ...)
}

 \arguments{
       \item{this}{A QMatrix object.}
       \item{value}{A list with the events names and the associated rates.}
       \item{...}{Not used.}
 }

 \value{
       The QMatrix object (invisible).
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
