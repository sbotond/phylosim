%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getTotalRate.Site}
\alias{getTotalRate.Site}
\alias{Site.getTotalRate}
\alias{getTotalRate,Site-method}

 \title{Get the total active event rate}

 \description{
	Get the total active event rate.
	The total rate is the sum of the rates of all active events given the current state of the Site object.
 }

 \usage{
\method{getTotalRate}{Site}(this, ...)
}

 \arguments{
 	\item{this}{A Site object.}
 	\item{...}{Not used.}
 }

 \value{
 	A numeric vector of length one.
 }

 \examples{

	# create a nucleotide site with a JC69 substitution process attached
       s<-Site(state="A",alphabet=NucleotideAlphabet(),processes=list(JC69()))
       # get the total rate
	getTotalRate(s)
       # add a new process
       attachProcess(s,K80(rate.params=list("Alpha"=1,"Beta"=0.5)))
       # get the total rate via virtual field
       s$totalRate

 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Site}}.
 }
