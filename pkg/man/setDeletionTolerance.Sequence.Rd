%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setDeletionTolerance.Sequence}
\alias{setDeletionTolerance.Sequence}
\alias{Sequence.setDeletionTolerance}
\alias{setDeletionTolerance,Sequence-method}

 \title{Set the deletion tolerance site-process specific parameter for a collection of Site objects aggregated by a Sequence object}

 \description{
	Set the deletion tolerance site-process specific parameter for a collection of Site objects aggregated by a Sequence object.
	This method does some error checking and the calls \code{setParameterAtSites(this=this,process=process,id="deletion.tolerance",value=value,index=index)}.
	See \code{setParameterAtSites.Sequence} for more details.
 }

 \usage{
\method{setDeletionTolerance}{Sequence}(this, process, value, index, ...)
}

 \arguments{
 	\item{this}{A Sequence object.}
 	\item{process}{A Process object.}
 	\item{value}{A numeric vector, recycled if shorter than the index vector.}
	\item{index}{An integer vector specifying a set of positions. It is set to 1:seq$length if omitted.}
 	\item{...}{Not used.}
 }

 \value{
 	The Sequence object (invisible).
 }


 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	setParameterAtSites.Sequence
 }
