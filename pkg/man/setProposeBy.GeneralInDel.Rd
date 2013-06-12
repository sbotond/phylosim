%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setProposeBy.GeneralInDel}
\alias{setProposeBy.GeneralInDel}
\alias{GeneralInDel.setProposeBy}
\alias{setProposeBy,GeneralInDel-method}
 \alias{setProposeBy.GeneralInsertor}

 \title{Set the function used for proposing indel lengths}

 \description{
	Set the function used for proposing indel lengths.

	The function must return a numeric vector of length one. The function must have an
	argument named "process" which will hold the calling process object.
 }

 \usage{
\method{setProposeBy}{GeneralInDel}(this, value, ...)
}

 \arguments{
 	\item{this}{A GeneralInDel object.}
	\item{value}{A function object returning a numeric vector of length one.}
 	\item{...}{Not used.}
 }

 \value{
 	The function object (invisible).
 }

 \examples{
	# create a GeneralInDel object
	# proposing events with a constant length of 5
	o<-GeneralInDel(rate=1, propose.by=function(process){return(5)});
	# set/get the proposeBy function
	setProposeBy(o,value=function(process){return(6)})
	getProposeBy(o)
	# set/get proposeBy function via virtual field
	o$proposeBy<-function(process){return(3)}
	o$proposeBy
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{GeneralInDel}}.
 }
