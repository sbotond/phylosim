%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{plot.ContinuousInsertor}
\alias{plot.ContinuousInsertor}
\alias{ContinuousInsertor.plot}
\alias{plot,ContinuousInsertor-method}

 \title{Plot the density of proposed lengths}

 \description{
	Plot the density of proposed lengths.
 }

 \usage{
\method{plot}{ContinuousInsertor}(x, sample.size=NA, ...)
}

 \arguments{
 	\item{x}{A ContinuousInsertor object.}
       \item{sample.size}{Number of lengths sampled for the plot.}
 	\item{...}{Not used.}
 }

 \value{
 	The process object (invisible).
 }

 \examples{
	# create object
	o<-ContinuousInsertor(rate=1)
	# set length sampling expression via virtual field
	o$dist<-expression(rnorm(1,mean=10,sd=4))
	# set maxLength
	setMaxLength(o, 30)
	# plot density
	plot(o)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{ContinuousInsertor}}.
 }
