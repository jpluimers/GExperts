Building your own version of GExperts:

** PLEASE DO NOT DISTRIBUTE YOUR OWN VERSIONS WITHOUT CHANGING THE VERSION
INFORMATION FIRST! (see below) **

Once you have checked out all the source files, building your own version of
GExperts is quite simple:

1. switch to the "projects" folder
3. switch to the delphi version you are using
4. call "__build_project.cmd"

If I haven't made a mistake, this script should then compile a GExpertsXxx.dll
to the folder "Binaries" respectively.

Alternatively you can call "__build.cmd" in the root of the source tree followed by a Delphi version:

    6
    7
    2005
    2006
    2007
    2009
    2010
    XE1
    XE2
    XE3
    XE4
    XE5
    XE6
    XE7
    XE8
    Xx10Seattle
    Xx101Berlin

To build GExperts for all supported Delphi versions, call
"__build.cmd" in the root of the source tree.

To change the version information prior to building, edit the file
"GExperts_version.ini" in the "projects" directory
This file is used by all projects so you only need to edit this.

Like "__build.cmd", call "__Release.cmd" with or without a version parameter to 
create a "GExperts-experimental-twm.zip" file that contains all the files 
necessary to install the experimental GExperts.


**Dependencies**

"__build.cmd" and "__build_project.cmd" of course require the Delphi versions
you want to compile for. Note that some Editions (e.g. "Starter") do not come
with the required command line compiler, so the build scripts won't work with
them. But you can always open the projects in the IDE and compile them there.
