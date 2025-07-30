## Test environments

* local macOS R installation, R 4.5.1
* continuous integration via GH actions:
  * macOS latest release
  * windows latest release
  * ubuntu 24.04.2 LTS and devel and oldrel-1
* [win-builder](https://win-builder.r-project.org/) (release and devel)
* [macOS-builder](https://mac.r-project.org/macbuilder/submit.html)
* [R-hub](https://r-hub.github.io/rhub/): All platforms expect
  * `gcc14` for which package {pcaPP} fails to install;
  * `intel` which fails to install because it cannot find `libquadmath.so` required by {TDAvec} dependency;
  * `mkl` which fails to install because it cannot find symbol `mkl_lapack_dcombssq` required by {TDAvec} dependency.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
