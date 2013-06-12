%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{is.GeneralInsertor}
\alias{is.GeneralInsertor.default}
\alias{is.GeneralInsertor}

 \title{Check whether an object inherits from GeneralInsertor}

 \description{
		Check whether an object inherits from GeneralInsertor.
 }

 \usage{
\method{is.GeneralInsertor}{default}(this, ...)
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
	# check if they inherit from GeneralInsertor
	is.GeneralInsertor(i)
	is.GeneralInsertor(d)
 }

 \author{Botond Sipos, Gregory Jordan}

