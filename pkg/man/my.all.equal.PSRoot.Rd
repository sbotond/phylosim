%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{my.all.equal.PSRoot}
\alias{my.all.equal.PSRoot}
\alias{PSRoot.my.all.equal}
\alias{my.all.equal,PSRoot-method}

 \title{Test if two objects are nearly equal}

 \description{
	Test if two objects are nearly equal.

	This method simply calls \code{all.equal.default} with the tolerance parameter set to
	\code{.Machine$double.eps ^ 0.5}. More useful as a static method.

 }

 \usage{
\method{my.all.equal}{PSRoot}(static, target, current, ...)
}

 \arguments{
 	\item{static}{A PSRoot object.}
	\item{target}{R object.}
	\item{current}{Other R object, to be compared with target.}
 	\item{...}{Not used.}
 }

 \value{
	TRUE or FALSE.
 }

 \examples{
	PSRoot$my.all.equal(0.0,0.0001)
	PSRoot$my.all.equal(0.0,0.000000001)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PSRoot}}.
 }
