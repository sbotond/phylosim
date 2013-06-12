%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getTransTable.CodonAlphabet}
\alias{getTransTable.CodonAlphabet}
\alias{CodonAlphabet.getTransTable}
\alias{getTransTable,CodonAlphabet-method}

 \title{Get the list storing the genetic code table}

 \description{
	Get the list storing the genetic code table.
 }

 \usage{
\method{getTransTable}{CodonAlphabet}(this, ...)
}

 \arguments{
 	\item{this}{A CodonAlphabet object.}
 	\item{...}{Not used.}
 }

 \value{
 	A list.
 }

 \examples{
	# create object
	a<-CodonAlphabet()
	# get genetic code table
	getTransTable(a)
	# get genetic code table via virtual field
	a$transTable
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{CodonAlphabet}}.
 }
