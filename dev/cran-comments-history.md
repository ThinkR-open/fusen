# v0.7.0
## R CMD check results

0 errors | 0 warnings | 1 note

* Maintainer was changed for Vincent Guyader, instead Sébastien Rochette.
* OK on GitHub Actions, rhub, winbuilder and macbuilder
* Fixes error on CRAN checks: "invalid regular expression"




# v0.6.0
## R CMD check results

0 errors | 0 warnings | 1 note

* Maintainer email was changed for my personal address, instead of my professional one.
* OK on GitHub Actions, rhub, winbuilder and macbuilder
* Fixes notes on CRAN checks: "Lost braces; missing escapes or markup?"

# v0.5.2
## R CMD check results

0 errors | 0 warnings | 0 note

* This is a new release.

### Comment about `packageVersion()`

Thus, With c84610 R-devel now *warns* about such inputs, which will
typically not show in the check results (but immediately giving errors
would cause too much disruption).  Some of these warnings can be found
by code analyis.  I list these uses below, can you please fix as
necessary?  E.g., for the first issue shown,

  packageVersion("ggplot2") >= 3.3

should be changed to

  packageVersion("ggplot2") >= "3.3"

etc.

=> Fixed where needed. Verified with r-devel on GitHub Actions and winbuilder-devel

# v0.5.1
## R CMD check results

0 errors | 0 warnings | 0 note

* Tested on rhub, winbuilder and macbuilder

### Comments

- Found the following (possibly) invalid URLs:
     URL: https://codecov.io/gh/ThinkR-open/fusen (moved to
https://app.codecov.io/gh/ThinkR-open/fusen)
       From: README.md
       Status: 301
       Message: Moved Permanently

Please change http --> https, add trailing slashes, or follow moved
content as appropriate.

Please fix and resubmit.

=> Thank you. Sorry for the redirections, I do not always find the correct combination for codecov...
I followed the URL to use a not redirected content: `[![codecov](https://codecov.io/gh/ThinkR-open/fusen/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ThinkR-open/fusen)`  
I did not find 'http' calls though

# v0.4.2
## R CMD check results

0 errors | 0 warnings | 0 note

* This is a patch to fix problem with the new version of {usethis}
* This fixes errors in https://cran.r-project.org/web/checks/check_results_fusen.html
* Tested on rhub, winbuilder and macbuilder

# v0.4.1
## R CMD check results

0 errors | 0 warnings | 0 note

* This is a new release.
