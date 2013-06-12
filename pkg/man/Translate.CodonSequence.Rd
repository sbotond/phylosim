%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{Translate.CodonSequence}
\alias{Translate.CodonSequence}
\alias{CodonSequence.Translate}
\alias{Translate,CodonSequence-method}

 \title{Translate a CodonSequence object}

 \description{
	Translate a CodonSequence object.
 }

 \usage{
\method{Translate}{CodonSequence}(this, ...)
}

 \arguments{
 	\item{this}{A CodonSequence object}
 	\item{...}{Not used.}
 }

 \value{
 	The translation as an AminoAcidSequence object.
 }

 \examples{
	# create a CodonSequence object
	s<-CodonSequence(string="ATCTTTCGAATGGGGCCCTCCCGA")
	# get the translation as an AminoAcidSequence object
	as<-Translate(s)
	as
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{CodonSequence}}.
 }
