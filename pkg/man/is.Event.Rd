%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{is.Event}
\alias{is.Event.default}
\alias{is.Event}

 \title{Check whether an object inherits from the class Event}

 \description{
	Check whether an object inherits from the class Event.
 }

 \usage{
\method{is.Event}{default}(this, ...)
}

 \arguments{
 	\item{this}{An object.}
 	\item{...}{Not used.}
 }

 \value{
 	TRUE or FALSE.
 }

 \examples{
	# create some objects
	e<-Event(); a<-Alphabet()
	# check if they inherit from Event
	is.Event(e)
	is.Event(a)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Event}}.
 }

