%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setGenerateBy.GeneralInsertor}
\alias{setGenerateBy.GeneralInsertor}
\alias{GeneralInsertor.setGenerateBy}
\alias{setGenerateBy,GeneralInsertor-method}

 \title{Set the function object used for generating inserts}

 \description{
	Set the function object used for generating inserts.
	The provided function must return a Sequence object whne called and must have the
	following arguments: process, length, target.seq, event.pos, insert.pos (see \code{generateInsert.GeneralInsertor}).
 }

 \usage{
\method{setGenerateBy}{GeneralInsertor}(this, value, ...)
}

 \arguments{
 	\item{this}{A GeneralInsertor object.}
	\item{value}{A function object.}
 	\item{...}{Not used.}
 }

 \value{
 	The function object.
 }

 \examples{
	# create a GeneralInsertor object
	i<-GeneralInsertor(
		rate=0.5,
		propose.by=function(process){sample(c(5:10),1)}, # inserts between 5 and 10
		template.seq=NucleotideSequence(string="AAAAAAA")
	)

	# save insert generator
	old.gen<-getGenerateBy(i)
	# set a new insert generator
	i$generateBy<-function(
                           process,
                           length,
                           target.seq,
                           event.pos,
                           insert.pos){
                   return(NucleotideSequence(string="AATTGGCC"))
                   }
	# get the generator function
	i$generateBy
	# generate insert
	generateInsert(i)
	# restore old generator
	i$generateBy<-old.gen
	# generate insert
	generateInsert(i)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{GeneralInsertor}}.
 }
