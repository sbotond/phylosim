%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setComments.PSRoot}
\alias{setComments.PSRoot}
\alias{PSRoot.setComments}
\alias{setComments,PSRoot-method}

 \title{Set the comments associated with an object}

 \description{
	Set the comments associated with an object.

	The comment field can contain any type of object.
 }

 \usage{
\method{setComments}{PSRoot}(this, new_value, ...)
}

 \arguments{
 	\item{this}{A PSRoot object.}
	\item{new_value}{An object.}
 	\item{...}{Not used.}
 }

 \value{
 	The new value of the comment field (invisible).
 }

 \examples{
	# create an object
	o<-PSRoot()
	# add some comments
	setComments(o,"Random comment")
	# get the comment
	getComments(o)
	# get/set the comment via virtual fiels
	o$comments<-"Second random comment"
	o$comments
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{PSRoot}}.
 }
