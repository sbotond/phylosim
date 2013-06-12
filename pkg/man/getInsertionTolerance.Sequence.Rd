%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getInsertionTolerance.Sequence}
\alias{getInsertionTolerance.Sequence}
\alias{Sequence.getInsertionTolerance}
\alias{getInsertionTolerance,Sequence-method}

 \title{Get the insertion tolerance site-process specific parameter for a collection of Site objects aggregated by a Sequence object}

 \description{
	Get the insertion tolerance site-process specific parameter for a collection of Site objects aggregated by a Sequence object.
	This method does some error checking and the calls \code{getParameterAtSites(this=this,process=process,id="insertion.tolerance",index=index)}.
	See \code{getParameterAtSites.Sequence} for more details.
 }

 \usage{
\method{getInsertionTolerance}{Sequence}(this, process, index, ...)
}

 \arguments{
 	\item{this}{A Sequence object.}
 	\item{process}{A Process object.}
	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
 	\item{...}{Not used.}
 }

 \value{
 	A numeric vector.
 }


 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	getParameterAtSites.Sequence
 }
