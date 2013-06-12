%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setName.Process}
\alias{setName.Process}
\alias{Process.setName}
\alias{setName,Process-method}

 \title{Set the name of a Process object}

 \description{
	Set the name of a Process object.

	This method also updates the unique identifier of the Process object.
 }

 \usage{
\method{setName}{Process}(this, new.name, ...)
}

 \arguments{
 	\item{this}{A Process object.}
 	\item{new.name}{A character vector of length one.}
 	\item{...}{Not used.}
 }

 \value{
 	Returns the new name (invisible).
 }

 \examples{
	# create object
	p<-Process()
	# get name
	p$name
	# set new name
	getName(p)
	# get name and id
	p$name
	p$id
	# set new name via virtual field
	p$name<-"Procey"
	p$name
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Process}}.
 }
