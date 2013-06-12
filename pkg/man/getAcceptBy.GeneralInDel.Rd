%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getAcceptBy.GeneralInDel}
\alias{getAcceptBy.GeneralInDel}
\alias{GeneralInDel.getAcceptBy}
\alias{getAcceptBy,GeneralInDel-method}
 \alias{getAcceptBy.GeneralInsertor}

 \title{Get the function used for accepting/rejecting indel events}

 \description{
	Get the function used for accepting/rejecting indel events.
 }

 \usage{
\method{getAcceptBy}{GeneralInDel}(this, ...)
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
	# rejecting half of the events
	o<-GeneralInDel(
                   rate=1,
                   propose.by=function(process){return(5)},
                   accept.by=function( ){sample(c(TRUE,FALSE),1)}
                   );
	# set/get the acceptBy function
	setAcceptBy(o,value=function(){return(FALSE)})	# reject all events
	getAcceptBy(o)
	# set/get acceptBy function via virtual field
	o$acceptBy<-function(){return(TRUE)}		# accept all events
	o$acceptBy
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{GeneralInDel}}.
 }
