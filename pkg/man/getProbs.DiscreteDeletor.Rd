%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getProbs.DiscreteDeletor}
\alias{getProbs.DiscreteDeletor}
\alias{DiscreteDeletor.getProbs}
\alias{getProbs,DiscreteDeletor-method}

 \title{Get the deletion length probabilities}

 \description{
	Get the deletion length probabilities.
 }

 \usage{
\method{getProbs}{DiscreteDeletor}(this, ...)
}

 \arguments{
 	\item{this}{A DiscreteDeletor object.}
 	\item{...}{Not used.}
 }

 \value{
 	A numeric vector with the deletion length probabilities.
 }

 \examples{
	# create a DiscreteDeletor object
	d<-DiscreteDeletor(rate=1, sizes=1:3)
	# set/get length probabilities
	setProbs(d,c(1/3,1/3,1/3)) # equal probabilites
	getProbs(d)
	# set/get length probabilities via virtual field
	x<-c(2,2,1)
	# normalize x
	x<-x/sum(x)
	d$probs<-x
	d$probs
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{DiscreteDeletor}}.
 }
