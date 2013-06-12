%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{plot.GeneralSubstitution}
\alias{plot.GeneralSubstitution}
\alias{GeneralSubstitution.plot}
\alias{plot,GeneralSubstitution-method}

 \title{Create a bubble plot of the substitution process}

 \description{
	Create a bubble plot of the substitution process.

	Bubble plots (\url{http://biowiki.org/BubblePlots}) visualize the characteristics of the
	substitution process. The area of the circles is proportional to the rates/probabilities.
	The plot is not produced if the rate matrix or the equlibrium
	distribution has undefined elements.
 }

 \usage{
\method{plot}{GeneralSubstitution}(x, scale=1, ...)
}

 \arguments{
 	\item{x}{An object inheriting from GeneralSubstitution.}
	\item{scale}{A scale factor affecting the area of the circles.}
 	\item{...}{Not used.}
 }

 \value{
 	The process object (invisible).
 }

 \examples{
	plot(BinarySubst(rate.list=list("0->1"=1,"1->0"=1.5)))
	plot(JC69())
	# get smaller circles
	plot(JC69(),scale=0.5)
	plot(F84(base.freqs=c(3/6,1/6,1/6,1/6)))
	plot(WAG())
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{GeneralSubstitution}}.
 }
