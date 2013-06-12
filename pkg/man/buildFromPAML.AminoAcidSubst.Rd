%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{buildFromPAML.AminoAcidSubst}
\alias{buildFromPAML.AminoAcidSubst}
\alias{AminoAcidSubst.buildFromPAML}
\alias{buildFromPAML,AminoAcidSubst-method}

 \title{Build rate matrix from PAML file}

 \description{
	Build rate matrix from PAML file.
 }

 \usage{
\method{buildFromPAML}{AminoAcidSubst}(this, paml.file, ...)
}

 \arguments{
 	\item{this}{An AminoAcidSubst object.}
	\item{paml.file}{Path to the PAML file.}
 	\item{...}{Not used.}
 }

 \value{
 	The AminoAcidSubst object (invisible).
 }

 \examples{
	# create an object
	p<-AminoAcidSubst()
	# build rate matrix from paml file
	# buildFromPAML(p,"path_to_paml_file")	# do not run this
	# get object summary
	summary(p)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{AminoAcidSubst}}.
 }
