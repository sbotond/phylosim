%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setParameterAtSite.Process}
\alias{setParameterAtSite.Process}
\alias{Process.setParameterAtSite}
\alias{setParameterAtSite,Process-method}
 \alias{setParameterAtSite.FastFieldDeletor}
 \title{Set the value of a site-process specific paramter in a Site object attached to a Process object}

 \description{
	Set the value of a site-process specific paramter in a Site object attached to a Process object.
	The Process object must be attached to the Site object. The new value must be compatible with the type
	of the site-process specific parameter.

 }

 \usage{
\method{setParameterAtSite}{Process}(this, site, id, value, ...)
}

 \arguments{
 	\item{this}{A Process object.}
 	\item{site}{A Site object.}
	\item{id}{The identifier of the site-process specific parameter.}
	\item{value}{The new value for the parameter.}
 	\item{...}{Not used.}
 }

 \value{
 	The site-process specific parameter, which is a list containing the following keys: id, name, value, type.
 }

 \examples{
	# create a Site and a Process object
	a<-BinaryAlphabet()
	s<-Site(alphabet=a)
	p<-Process(alphabet=a)
	# attach the process
	attachProcess(s,p)
	# set the value of the rate multiplier
	setParameterAtSite(p,s,"rate.multiplier",2)
	# get the value of the rate multiplier
	getParameterAtSite(p,s,"rate.multiplier")
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Process}}.
 }
