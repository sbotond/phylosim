%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getComments.PSRoot}
\alias{getComments.PSRoot}
\alias{PSRoot.getComments}
\alias{getComments,PSRoot-method}

 \title{Get the comments associated with an object}

 \description{
	Get the comments associated with an object.

	The comment field can contain any type of object.
 }

 \usage{
\method{getComments}{PSRoot}(this, ...)
}

 \arguments{
 	\item{this}{A PSRoot object.}
 	\item{...}{Not used.}
 }

 \value{
 	The value of the comment field.
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
