%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{Process}
\docType{class}
\alias{Process}


 \title{The Process class}

 \description{

	This is the class representing a generic process acting on Site and Sequence objects. Process objects can be attached
	to Site objects if the associated Alphabet objects match.

	The processes can have site-process-specific parameters.
	The templates for site-process-specific parameters and their default values are stored in the Process objects and
       copied into the Site object when the process is attached. See the documentation of the Site class for more details.

	The rate multiplier parameter (id="rate.multiplier") is
	present in the Process class and is inherited by all descendant classes.

	Package:   \cr
\bold{Class Process}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link{PSRoot}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{Process}\cr

\bold{Directly known subclasses:}\cr
\link{AminoAcidSubst}, \link{BinarySubst}, \link{BrownianInsertor}, \link{CodonUNREST}, \link{ContinuousDeletor}, \link{ContinuousInsertor}, \link{cpREV}, \link{DiscreteDeletor}, \link{DiscreteInsertor}, \link{F81}, \link{F84}, \link{FastFieldDeletor}, \link{GeneralDeletor}, \link{GeneralInDel}, \link{GeneralInsertor}, \link{GeneralSubstitution}, \link{GTR}, \link{GY94}, \link{HKY}, \link{JC69}, \link{JTT}, \link{JTT.dcmut}, \link{K80}, \link{K81}, \link{LG}, \link{mtArt}, \link{mtMam}, \link{mtREV24}, \link{MtZoa}, \link{PAM}, \link{PAM.dcmut}, \link{T92}, \link{TN93}, \link{UNREST}, \link{WAG}\cr

public static class \bold{Process}\cr
extends \link{PSRoot}\cr


 }

 \usage{
Process(name=NA, alphabet=NA, ...)
}

 \arguments{
 	\item{name}{The name of the Process object: a character vector of length one.}
	\item{alphabet}{The associated Alphabet object.}
 	\item{...}{Not used.}
	}

 \section{Fields and Methods}{
 	\bold{Methods:}\cr
\tabular{rll}{
 \tab \code{!=} \tab  -\cr
 \tab \code{==} \tab  -\cr
 \tab \code{as.character} \tab  -\cr
 \tab \code{checkConsistency} \tab  -\cr
 \tab \code{clone} \tab  -\cr
 \tab \code{getAlphabet} \tab  -\cr
 \tab \code{getEventsAtSite} \tab  -\cr
 \tab \code{getId} \tab  -\cr
 \tab \code{getName} \tab  -\cr
 \tab \code{getParameterAtSite} \tab  -\cr
 \tab \code{getSiteSpecificParamIds} \tab  -\cr
 \tab \code{getSiteSpecificParamList} \tab  -\cr
 \tab \code{getWriteProtected} \tab  -\cr
 \tab \code{hasSiteSpecificParameter} \tab  -\cr
 \tab \code{hasUndefinedRate} \tab  -\cr
 \tab \code{is} \tab  -\cr
 \tab \code{setAlphabet} \tab  -\cr
 \tab \code{setId} \tab  -\cr
 \tab \code{setName} \tab  -\cr
 \tab \code{setParameterAtSite} \tab  -\cr
 \tab \code{setSiteSpecificParamIds} \tab  -\cr
 \tab \code{setSiteSpecificParamList} \tab  -\cr
 \tab \code{setWriteProtected} \tab  -\cr
 \tab \code{summary} \tab  -\cr
}

 }

 \examples{
	# create a Process object
	p<-Process(name="MyProc",alphabet=AminoAcidAlphabet())
	# check if it's a Process object
	is.Process(p)
	# check object consistency
	checkConsistency(p)
	# set process name
	p$name<-"Ppppproccc"
	# get process name
	p$name
	# get unique process identifier
	p$id
	# get the list of site specific paramters and paramter IDs
	p$siteSpecificParamList
	p$siteSpecificParamIds
	# get Process object summary
	summary(p)
	# clone process object
	pp<-clone(p)
	# test object identity
	p == p
	p == pp
	# create a site object
	s<-Site(alphabet=AminoAcidAlphabet())
	# attach process to Site object
	attachProcess(s,p)
	# get events at specified site
	getEventsAtSite(p,s)	# empty list
	# detach process via virtual field
	s$processes<-list()
	# attach processes via virtual field
	s$processes<-list(p,pp)
	# set the value of the rate multiplier for Site s
	setParameterAtSite(p,s,id="rate.multiplier",value=2)
	# get the value of the rate multiplier for Site s
	getParameterAtSite(p,s,id="rate.multiplier")
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{Process}}.
 }
