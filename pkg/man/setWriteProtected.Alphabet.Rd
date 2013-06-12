%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setWriteProtected.Alphabet}
\alias{setWriteProtected.Alphabet}
\alias{Alphabet.setWriteProtected}
\alias{setWriteProtected,Alphabet-method}

 \title{Set the write protection field for an object}

 \description{
	Set the write protection field for an object.
	Write protected objects cannot be modified through get/set methods and virtual fields.
 }

 \usage{
\method{setWriteProtected}{Alphabet}(this, value, ...)
}

 \arguments{
 	\item{this}{An object.}
 	\item{value}{A logical vector of size one.}
 	\item{...}{Not used.}
 }

 \value{
 	Invisible TRUE or FALSE.
 }

 \examples{

	# create an object
	o<-Alphabet()
	# toggle write protection
	setWriteProtected(o,TRUE)
	# check write protection
	o$writeProtected
	# set write protection via virtual field
	o$writeProtected<-FALSE
	o$writeProtected


 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Alphabet}}.
 }
