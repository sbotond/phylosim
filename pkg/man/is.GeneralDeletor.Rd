%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{is.GeneralDeletor}
\alias{is.GeneralDeletor.default}
\alias{is.GeneralDeletor}

 \title{Check whether an object inherits from GeneralDeletor}

 \description{
		Check whether an object inherits from GeneralDeletor.
 }

 \usage{
\method{is.GeneralDeletor}{default}(this, ...)
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
	d<-GeneralDeletor()
	i<-GeneralInsertor()
	# check if they inherit from GeneralDeletor
	is.GeneralDeletor(d)
	is.GeneralDeletor(i)
 }


 \author{Botond Sipos, Gregory Jordan}

