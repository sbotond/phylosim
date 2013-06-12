%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{flagTotalRate.Site}
\alias{flagTotalRate.Site}
\alias{Site.flagTotalRate}
\alias{flagTotalRate,Site-method}

 \title{Flag the total event rate}

 \description{
	Flag the total event rate.
	This method sets the cached total active event rate to NA, which will trigger its
	recalculation when next accessed via getTotalRate.
 }

 \usage{
\method{flagTotalRate}{Site}(this, ...)
}

 \arguments{
 	\item{this}{A Site object.}
 	\item{...}{Not used.}
 }

 \value{
 	Invisible NA.
 }

 \examples{
	# create a site object:
	p<-K80(rate.params=list("Alpha"=2,"Beta"=0.5))
	s<-Site(alphabet=NucleotideAlphabet(), state="G", processes=list(p))
	# get site rate
	s$totalRate
	# modifying site object in a dangerous way (do not do this under any circumstances!)
	s$.processes = list()	# site object is now inconsistent!
	# get cached rate
	s$totalRate		# incorrect value
	# flag total rate
	flagTotalRate(s)
	# get site rate
	s$totalRate		# correct value

 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Site}}.
 }
