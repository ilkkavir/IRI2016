.onAttach <- function(libname,pkgname){
  # set an environment variable IRIPATH that points to the directory where
  # IRI input data files are stored
  Sys.setenv(IRIPATH=system.file(package='IRI2016','extdata'))
  # the model requires certain initialisations, which are collected in subroutine init()
  .Fortran("init")
cat("\nCopyright (c) 2016 International Reference Ionosphere\n\n",
'THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE IRI AUTHORS OR THE IRI WORKING GROUP MEMBERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.\n\n',
'Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to use, copy, and modify the Software subject to the following conditions: The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software and the IRI Working Group should be acknowledge in scientific papers that make use of the Software and the scientific paper describing the specific version of the IRI model should be referenced.\n',
    fill=50
    )
}
