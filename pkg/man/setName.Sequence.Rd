%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setName.Sequence}
\alias{setName.Sequence}
\alias{Sequence.setName}
\alias{setName,Sequence-method}

 \title{Set the name of a Sequence object}

 \description{
	Set the name of a Sequence object.
 }

 \usage{
\method{setName}{Sequence}(this, new.name, ...)
}

 \arguments{
 	\item{this}{A Sequence object.}
 	\item{new.name}{A character vector of length one.}
 	\item{...}{Not used.}
 }

 \value{
 	The new object name (invisible).
 }

 \examples{
	# create a Sequence object
	s<-Sequence(name="MySeq",length=4)
	# get sequence name
	s$name
	# rename object
	setName(s,"SO")
	s$name
	# rename via virtual field
	s$name<-"SeqSeq"
	s$name
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Sequence}}.
 }
