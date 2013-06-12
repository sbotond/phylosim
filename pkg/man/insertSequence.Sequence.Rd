%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{insertSequence.Sequence}
\alias{insertSequence.Sequence}
\alias{Sequence.insertSequence}
\alias{insertSequence,Sequence-method}

 \title{Insert a Sequence object into another Sequence object after a specified position}

 \description{
	Insert a Sequence object into another Sequence object after a specified position.
 }

 \usage{
\method{insertSequence}{Sequence}(this, insert, position, process=NA, paranoid=FALSE, ...)
}

 \arguments{
 	\item{this}{The target Sequence object.}
 	\item{insert}{The Sequence object to be inserted.}
 	\item{position}{The position after the Sequence object will be inserted.}
 	\item{process}{The Process object performing the insertion (optional).}
 	\item{paranoid}{If TRUE, then the consistency of teh target objects is checked more rigurously after insertion.}
 	\item{...}{Not used.}
 }

 \value{
 	The Sequence object (invisible).
 }

 \examples{
	# create some sequence objects.
	target<-NucleotideSequence(string="AAAAAAAAAAA")
	insert<-NucleotideSequence(string="GGGGGGGGGGG")
	# insert after position 5
	insertSequence(target,insert,5)
	# print the target sequence
	target
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Sequence}}.
 }
