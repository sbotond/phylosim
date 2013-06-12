%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setState.Site}
\alias{setState.Site}
\alias{Site.setState}
\alias{setState,Site-method}

 \title{Set the state of a Site object}

 \description{
	Set the state of a Site object.
 }

 \usage{
\method{setState}{Site}(this, new.state, ...)
}

 \arguments{
 	\item{this}{A Site object.}
 	\item{new.state}{A character vector of length one, containing a symbol belonging to the attached Alphabet object.}
 	\item{...}{Not used.}
 }

 \value{
 	Returns the new state (invisible).
 }

 \examples{

       # create a Site object with an Alphabet object attached
       s<-Site(alphabet=Alphabet(symbols=c(0,1)), state=1);
       # set a new state
       setState(s,"0")
       # get state via virtual field
       s$state
       # set a new state via virtual field
       s$state<-1
	s$state

 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Site}}.
 }
