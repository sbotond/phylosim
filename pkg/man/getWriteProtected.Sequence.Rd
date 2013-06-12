%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getWriteProtected.Sequence}
\alias{getWriteProtected.Sequence}
\alias{Sequence.getWriteProtected}
\alias{getWriteProtected,Sequence-method}

 \title{Check if the object is write protected}

 \description{
	Check if the object is write protected.
	Write protected objects cannot be modified through get/set methods and virtual fields.
 }

 \usage{
\method{getWriteProtected}{Sequence}(this, ...)
}

 \arguments{
 	\item{this}{An object.}
 	\item{...}{Not used.}
 }

 \value{
 	TRUE or FALSE
 }

 \examples{

       # create an object
       o<-Sequence()
       # toggle write protection
       o$writeProtected<-TRUE
       # check if it's write protected
       getWriteProtected(o)
       # check write protection via virtual field
       o$writeProtected

 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Sequence}}.
 }
