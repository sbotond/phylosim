%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setAcceptBy.GeneralInDel}
\alias{setAcceptBy.GeneralInDel}
\alias{GeneralInDel.setAcceptBy}
\alias{setAcceptBy,GeneralInDel-method}
 \alias{setAcceptBy.GeneralInsertor}

 \title{Set the function used for accepting/rejecting indel events}

 \description{
	Set the function used for accepting/rejecting indel events.

	The function object must have the following arguments: process (the caller process), sequence (the target sequence),
	window (a vector containing the positions affecting acceptance).
 }

 \usage{
\method{setAcceptBy}{GeneralInDel}(this, value, ...)
}

 \arguments{
 	\item{this}{A GeneralInDel object.}
	\item{value}{A function object.}
 	\item{...}{Not used.}
 }

 \value{
 	The function object (invisible).
 }

 \examples{
	# create a GeneralInDel object
	# rejecting half of the events
	o<-GeneralInDel(
                   rate=1,
                   propose.by=function(process){return(5)},
                   accept.by=function( ){sample(c(TRUE,FALSE),1)}
                   );
	# set/get the acceptBy function
	setAcceptBy(o,value=function( ){return(FALSE)})	# reject all events
	getAcceptBy(o)
	# set/get acceptBy function via virtual field
	o$acceptBy<-function(){return(TRUE)}		# accept all events
	o$acceptBy
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{GeneralInDel}}.
 }
