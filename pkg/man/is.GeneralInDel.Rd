%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{is.GeneralInDel}
\alias{is.GeneralInDel.default}
\alias{is.GeneralInDel}

 \title{Check if an object inherits from the GeneralInDel class}

 \description{
		Check if an object inherits from the GeneralInDel class.
 }

 \usage{
\method{is.GeneralInDel}{default}(this, ...)
}

 \arguments{
		\item{this}{An object.}
		\item{...}{Not used.}

 }

 \value{
	TRUE or FALSE.
 }

 \examples{
	# create some objects
	o<-GeneralInDel(rate=1, propose.by=function(process){sample(c(1:10),1)});
	x<-GTR()
	# check if they inherit from GeneralInDel
	is.GeneralInDel(o)
	is.GeneralInDel(x)
 }


 \author{Botond Sipos, Gregory Jordan}

