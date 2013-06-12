%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setTemplateSeq.GeneralInsertor}
\alias{setTemplateSeq.GeneralInsertor}
\alias{GeneralInsertor.setTemplateSeq}
\alias{setTemplateSeq,GeneralInsertor-method}

 \title{Set the template sequence object}

 \description{
	Set the template sequence object.
	The template sequence object is used by the default \code{generateBy} function
	to generate insert sequences.
 }

 \usage{
\method{setTemplateSeq}{GeneralInsertor}(this, value, ...)
}

 \arguments{
 	\item{this}{A GeneralInsertor object.}
	\item{value}{A Sequence object.}
 	\item{...}{Not used.}
 }

 \value{
 	The Sequence object.
 }

 \examples{
	# create a GeneralInsertor object
	i<-GeneralInsertor(
		rate=0.5,
		propose.by=function(process){sample(c(5:10),1)}, # inserts between 5 and 10
		template.seq=NucleotideSequence(string="AAAAAAA")
	)
	# get template sequence
	getTemplateSeq(i)
	# get template sequence via virtual field
	i$templateSeq
	# set template sequence
	setTemplateSeq(i, NucleotideSequence(string="C"));
	# generate insert
	generateInsert(i)
	# set template sequence via virtual field
	i$templateSeq<-NucleotideSequence(string="G")
	# generate insert
	generateInsert(i)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{GeneralInsertor}}.
 }
