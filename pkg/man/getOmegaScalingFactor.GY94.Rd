%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getOmegaScalingFactor.GY94}
\alias{getOmegaScalingFactor.GY94}
\alias{GY94.getOmegaScalingFactor}
\alias{getOmegaScalingFactor,GY94-method}

 \title{Get the omega scaling factor}

 \description{
	Get the omega scaling factor.

       The rate matrix of the \code{\link{GY94}} model is scaled in a way that the expected number
       of potential substiutions per site is equal to one at equlibrium.
       The \emph{codeml} program from the PAML package scales the rate matrix in order to have
       the expected number of accepted substiutions per site equal to one.

	This method calculates the branch length multiplier needed for switching
	to PAML-style scaling given a fixed omega.

 }

 \usage{
\method{getOmegaScalingFactor}{GY94}(this, omega, ...)
}

 \arguments{
 	\item{this}{A GY94 object.}
	\item{omega}{The value of omega.}
 	\item{...}{Not used.}
 }

 \value{
 	A numeric vector of length one.
 }

 \examples{
	# construct a GY94 process object
	p<-GY94(kappa=4)
	# Calculate scaling factor for omega=2
	getOmegaScalingFactor(p,omega=2)
 }

 \author{Botond Sipos, Gregory Jordan}

 \seealso{
 	For more information see \code{\link{GY94}}.
 }
