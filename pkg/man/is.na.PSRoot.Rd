%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{is.na.PSRoot}
\alias{is.na.PSRoot}
\alias{PSRoot.is.na}
\alias{is.na,PSRoot-method}

 \title{Check if a PSRoot object is NA}

 \description{
	Check if a PSRoot object is NA.
	PSRoot objects accanot be NA, so this method always returns FALSE.
 }

 \usage{
\method{is.na}{PSRoot}(x, ...)
}

 \arguments{
 	\item{x}{A PSRoot object.}
 	\item{...}{Not used.}
 }

 \value{
 	FALSE
 }

 \examples{
	is.na(PSRoot());
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PSRoot}}.
 }
