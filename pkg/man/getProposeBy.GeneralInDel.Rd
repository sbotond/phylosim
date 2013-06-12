%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getProposeBy.GeneralInDel}
\alias{getProposeBy.GeneralInDel}
\alias{GeneralInDel.getProposeBy}
\alias{getProposeBy,GeneralInDel-method}
 \alias{getProposeBy.GeneralInsertor}

 \title{Get the function used for proposing indel lengths}

 \description{
	Get the function used for proposing indel lengths.
 }

 \usage{
\method{getProposeBy}{GeneralInDel}(this, ...)
}

 \arguments{
 	\item{this}{A GeneralInDel object.}
 	\item{...}{Not used.}
 }

 \value{
 	A function object.
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
