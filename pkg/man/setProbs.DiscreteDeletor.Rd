%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setProbs.DiscreteDeletor}
\alias{setProbs.DiscreteDeletor}
\alias{DiscreteDeletor.setProbs}
\alias{setProbs,DiscreteDeletor-method}

 \title{Set the deletion length probabilities}

 \description{
	Set the deletion length probabilities.

	The \code{sizes} virtual field must be set before setting the length probabilities.
	The length of the provided numeric vector must match with the length of the vector
	stored in the \code{sizes} virtual field. The vector is rescaled if the values do not
	sum to one and a warning is issued.
 }

 \usage{
\method{setProbs}{DiscreteDeletor}(this, value, ...)
}

 \arguments{
 	\item{this}{A DiscreteDeletor object.}
	\item{value}{A numeric vector containg the length probabilities.}
 	\item{...}{Not used.}
 }

 \value{
 	The vector of probabilities.
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
