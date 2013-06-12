%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{==.Alphabet}
\alias{==.Alphabet}
\alias{Alphabet.==}
\alias{==,Alphabet-method}
 \alias{!=.Alphabet}
 \title{Check if two alaphabet objects have the same symbol set}

 \description{
	Check if two alaphabet objects have the same symbol set.
 }

 \usage{
\method{==}{Alphabet}(e1, e2, ...)
}

 \arguments{
 	\item{e1}{An Alphabet object.}
 	\item{e2}{An Alphabet object.}
 	\item{...}{Not used.}
 }

 \value{
 	TRUE or FALSE
 }

 \examples{

	# create an alpahabet object
	a<-Alphabet(symbols=c(0,1));
	# clone object
	b<-clone(a)
	# compare the two objects
	print(a == b)
	# modify symbol set in b
	b$symbols<-c("AT","GC");
	print(a == b)

 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Alphabet}}.
 }
