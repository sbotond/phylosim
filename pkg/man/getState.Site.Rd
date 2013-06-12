%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getState.Site}
\alias{getState.Site}
\alias{Site.getState}
\alias{getState,Site-method}

 \title{Get the current state of a Site object}

 \description{
	Get the current state of a Site object.
 }

 \usage{
\method{getState}{Site}(this, ...)
}

 \arguments{
 	\item{this}{A Site object.}
 	\item{...}{Not used.}
 }

 \value{
 	A character vector of length one contaning the state (a symbol belonging to the attached Alphabet object).
 }

 \examples{

	# create a Site object with an Alphabet object attached
	s<-Site(alphabet=Alphabet(symbols=c(0,1)), state=1);
	# get current state
	getState(s)
	# get state via virtual field
	s$state

 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Site}}.
 }
