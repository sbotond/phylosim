%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{proposeLength.GeneralInDel}
\alias{proposeLength.GeneralInDel}
\alias{GeneralInDel.proposeLength}
\alias{proposeLength,GeneralInDel-method}

 \title{Propose indel length}

 \description{
	Propose indel length.

	This method simply calls the function returned by the \code{getProposeBy} method.
 }

 \usage{
\method{proposeLength}{GeneralInDel}(this, ...)
}

 \arguments{
 	\item{this}{A GeneralInDel object.}
 	\item{...}{Not used.}
 }

 \value{
 	A numeric vector of length one (the indel length).
 }

 \examples{
	# create a GeneralInDel object
	# proposing event lengths in the range 1:10
	o<-GeneralInDel(rate=1, propose.by=function(process){sample(c(1:10),1)});
	# propose indel length
	proposeLength(o)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{GeneralInDel}}.
 }
