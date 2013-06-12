%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getParameterAtSite.Process}
\alias{getParameterAtSite.Process}
\alias{Process.getParameterAtSite}
\alias{getParameterAtSite,Process-method}

 \title{Get the value of a site-process specific paramter from a Site object attached to a Process object}

 \description{
	Get the value of a site-process specific paramter from a Site object attached to a Process object.
	The Process object must be attached to the Site object.
 }

 \usage{
\method{getParameterAtSite}{Process}(this, site, id, ...)
}

 \arguments{
 	\item{this}{A Process object.}
 	\item{site}{A Site object.}
	\item{id}{The identifier of the site-process specific parameter.}
 	\item{...}{Not used.}
 }

 \value{
 	The site-process specific parameter, which is a list containing the following keys: id, name, value, type.
 }

 \examples{
	# create a Site and a Process object
	a<-Alphabet()
	s<-Site(alphabet=a)
	p<-Process(alphabet=a)
	# attach the process
	attachProcess(s,p)
	# get the value of the rate multiplier
	getParameterAtSite(p,s,"rate.multiplier")
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Process}}.
 }
