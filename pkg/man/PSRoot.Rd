%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{PSRoot}
\docType{class}
\alias{PSRoot}


 \title{The root class for all phylosim objects}

 \description{
		The root class for all phylosim objects containig some utility methods.
		Package:   \cr
\bold{Class PSRoot}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{PSRoot}\cr

\bold{Directly known subclasses:}\cr
\link{Alphabet}, \link{AminoAcidAlphabet}, \link{AminoAcidSequence}, \link{AminoAcidSubst}, \link{AnyAlphabet}, \link{BinaryAlphabet}, \link{BinarySequence}, \link{BinarySubst}, \link{BrownianInsertor}, \link{CodonAlphabet}, \link{CodonSequence}, \link{CodonUNREST}, \link{ContinuousDeletor}, \link{ContinuousInsertor}, \link{cpREV}, \link{DiscreteDeletor}, \link{DiscreteInsertor}, \link{Event}, \link{F81}, \link{F84}, \link{FastFieldDeletor}, \link{GeneralDeletor}, \link{GeneralInDel}, \link{GeneralInsertor}, \link{GeneralSubstitution}, \link{GTR}, \link{GY94}, \link{HKY}, \link{JC69}, \link{JTT}, \link{JTT.dcmut}, \link{K80}, \link{K81}, \link{LG}, \link{mtArt}, \link{mtMam}, \link{mtREV24}, \link{MtZoa}, \link{NucleotideAlphabet}, \link{NucleotideSequence}, \link{PAM}, \link{PAM.dcmut}, \link{PhyloSim}, \link{Process}, \link{QMatrix}, \link{Sequence}, \link{Site}, \link{T92}, \link{TN93}, \link{UNREST}, \link{WAG}\cr

public static class \bold{PSRoot}\cr
extends \link[R.oo]{Object}\cr


 }

 \usage{
PSRoot(...)
}

 \arguments{
   \item{...}{Not used.}
 }

 \section{Fields and Methods}{
 	\bold{Methods:}\cr
\tabular{rll}{
 \tab \code{checkConsistency} \tab  -\cr
 \tab \code{enableVirtual} \tab  -\cr
 \tab \code{getComments} \tab  -\cr
 \tab \code{getMethodsList} \tab  -\cr
 \tab \code{globalConsistencyCheck} \tab  -\cr
 \tab \code{intersect.list} \tab  -\cr
 \tab \code{is.na} \tab  -\cr
 \tab \code{is} \tab  -\cr
 \tab \code{ll} \tab  -\cr
 \tab \code{my.all.equal} \tab  -\cr
 \tab \code{plot} \tab  -\cr
 \tab \code{setComments} \tab  -\cr
 \tab \code{setMethodsList} \tab  -\cr
 \tab \code{summary} \tab  -\cr
 \tab \code{virtualAssignmentForbidden} \tab  -\cr
}


 \bold{Methods inherited from Object}:\cr
[[, [[<-, $, $<-, as.character, attach, attachLocally, clearCache, clearLookupCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFieldModifier, getFieldModifiers, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save

 }

 \examples{
		obj<-PSRoot();
		ll(obj);
 }

 \author{Botond Sipos, Gregory Jordan}


 \seealso{
 	Object
 }
