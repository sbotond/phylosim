%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getGenerateBy.GeneralInsertor}
\alias{getGenerateBy.GeneralInsertor}
\alias{GeneralInsertor.getGenerateBy}
\alias{getGenerateBy,GeneralInsertor-method}

 \title{Get the function object used for generating inserts}

 \description{
	Get the function object used for generating inserts.
 }

 \usage{
\method{getGenerateBy}{GeneralInsertor}(this, ...)
}

 \arguments{
 	\item{this}{A GeneralInsertor object.}
 	\item{...}{Not used.}
 }

 \value{
 	A function object.
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
                           insert.pos
                        ){
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
