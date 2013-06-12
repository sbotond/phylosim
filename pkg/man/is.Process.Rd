%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{is.Process}
\alias{is.Process.default}
\alias{is.Process}

 \title{Check if an object is an instance of the Process class}

 \description{
	Check if an object is an instance of the Process class.
 }

 \usage{
\method{is.Process}{default}(this, ...)
}

 \arguments{
 	\item{this}{An object.}
 	\item{...}{Not used.}
 }

 \value{
 	TRUE or FALSE
 }

 \examples{
	# create some objects
	a<-Site();
	p<-Process()
	# check if they inherit from Process
	is.Process(a)
	is.Process(p)

 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Process}}.
 }

