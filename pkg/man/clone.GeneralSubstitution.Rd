%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{clone.GeneralSubstitution}
\alias{clone.GeneralSubstitution}
\alias{GeneralSubstitution.clone}
\alias{clone,GeneralSubstitution-method}

 \title{Clone a GeneralSubstitution object}

 \description{
	Clone a GeneralSubstitution object.

	This method also clones the aggregated QMatrix object, but not the aggregated Alphabet
	object, as that is a good target for recycling.
 }

 \usage{
\method{clone}{GeneralSubstitution}(this, ...)
}

 \arguments{
 	\item{this}{A GeneralSubstitution object.}
 	\item{...}{Not used.}
 }

 \value{
 	A GeneralSubstitution object.
 }

 \examples{
	# create a GeneralSubstitution object
	p<-GeneralSubstitution(
                           alphabet=BinaryAlphabet(),
                           rate.list=list("0->1"=1,"1->0"=2),
                           name="MyBinary"
                           )
	# clone p
	pp<-clone(p)
	# do some checks
	p;pp
	p == p
	p == pp
	equals(p$qMatrix, pp$qMatrix)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{GeneralSubstitution}}.
 }
