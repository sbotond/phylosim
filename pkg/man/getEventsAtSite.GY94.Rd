%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ../PhyloSimSource.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getEventsAtSite.GY94}
\alias{getEventsAtSite.GY94}
\alias{GY94.getEventsAtSite}
\alias{getEventsAtSite,GY94-method}

 \title{Generate the list of active Event objects for a given attached Site object}

 \description{
       Generate the list of active Event objects for a given attached Site object.

 This method is almost an exact duplicate of the getEventsAtSite.GeneralSubstitution,
 with the exception of the portions dealing with the omega site-process specific parameter.
     
 }

 \usage{
\method{getEventsAtSite}{GY94}(this, target.site, ...)
}

 \arguments{
       \item{this}{A GY94 object.}
       \item{target.site}{A Site object. The GY94 object must be attached to the Site object.}
       \item{...}{Not used.}
 }

 \value{
       A list of the active Event objects.
 }


 \author{Botond Sipos, Gregory Jordan}

 \seealso{
       getEventsAtSite.GeneralSubstitution GeneralSubstitution
 }
