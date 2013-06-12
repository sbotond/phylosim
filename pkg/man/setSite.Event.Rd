%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{setSite.Event}
\alias{setSite.Event}
\alias{Event.setSite}
\alias{setSite,Event-method}

 \title{Assotiate an Event object with a Site object}

 \description{
	Assotiate an Event object with a Site object.
 }

 \usage{
\method{setSite}{Event}(this, new.site, ...)
}

 \arguments{
 	\item{this}{An Event object.}
 	\item{new.site}{A valid Site object.}
 	\item{...}{Not used.}
 }

 \value{
 	The new associated Site object (invisible).
 }

 \examples{
	# create an Event object
	e<-Event()
	# create some Site objects
	s1<-Site(alphabet=NucleotideAlphabet(),state="A")
	s2<-clone(s1); s2$state<-"T"
	# assotiate s1 with e
	setSite(e,s1)
	e$site
	# assotiate s2 with e via virtual field
	e$site<-s2
	e$site
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Event}}.
 }
