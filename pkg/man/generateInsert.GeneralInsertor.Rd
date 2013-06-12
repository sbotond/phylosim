%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{generateInsert.GeneralInsertor}
\alias{generateInsert.GeneralInsertor}
\alias{GeneralInsertor.generateInsert}
\alias{generateInsert,GeneralInsertor-method}

 \title{Generate an insert}

 \description{
	Generate an insert.

	This method uses the function stgored in the \code{proposeBy} virtual field to
	sample the insert length and then calls the function stored in the \code{generateBy}
	field to generate the insert.

	The default \code{generateBy} function set by the GeneralInsertor constructor truncates/repeats
	the template sequence stored in the \code{templateSeq} field to have the sequence with the right size
	and then calls the \code{sampleStates} method on the resulting object. That means that if we start with a
	template sequence which has NA states, but has a substitution process attached, then the resulting sequence
	will be different every time the \code{generateInsert} method is called.

	The \code{generateBy}, \code{proposeBy} and \code{templateSeq} fields must be set in order to use this method.
 }

 \usage{
\method{generateInsert}{GeneralInsertor}(this, length=NA, target.seq=NA, event.pos=NA, insert.pos=NA, ...)
}

 \arguments{
 	\item{this}{A GeneralInsertor object.}
	\item{length}{Generate an insert with the specified length if this argument is present.}
	\item{target.seq}{The Sequence object targeted by the insertion (optional). This argument is passed to the \code{generateBy} method.}
	\item{event.pos}{The position of the site proposing the insertion (optional). This argument is passed to the \code{generateBy} method.}
	\item{insert.pos}{The position of the insertion in the target sequence (optional). This argument is passed to the \code{generateBy} method.}
 	\item{...}{Not used.}
 }

 \value{
 	A Sequence object.
 }

 \examples{
	# build the template sequence
	ts<-NucleotideSequence(length = 10,processes=list(list(JC69())));
	# fix some site states
	setStates(ts,"A",c(1,2));
	setStates(ts,"T",c(5,6));
	setStates(ts,"C",c(9,10));
	# print template sequence
	ts
	# create a GeneralInsertor object
	i<-GeneralInsertor(
		rate=0.5,
		template.seq=ts,
		propose.by=function(process){sample(c(5:50),1)}, # inserts between 5 and 50
	)
	# generate some inserts
	generateInsert(i)
	generateInsert(i)
	generateInsert(i)
	generateInsert(i)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{GeneralInsertor}}.
 }
