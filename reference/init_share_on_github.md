# Initiate GitHub to share your package on a website

This uses 'pkgdown' to share the documentation of the package through
GitHub Actions. You may need to run
[`usethis::create_github_token()`](https://usethis.r-lib.org/reference/github-token.html),
then
[`gitcreds::gitcreds_set()`](https://gitcreds.r-lib.org/reference/gitcreds_get.html)
before.

## Usage

``` r
init_share_on_github(ask = TRUE, organisation = NULL)
```

## Arguments

- ask:

  Logical. `TRUE` (default) to ask the user to apply the instructions
  each time needed, or `FALSE` if the user already know what to do.

- organisation:

  If supplied, the repo will be created under this organisation, instead
  of the login associated with the GitHub token discovered for this
  `host`. The user's role and the token's scopes must be such that you
  have permission to create repositories in this `organisation`.

## Value

The URL of the website created

## Details

`init_share_on_github()` runs multiple steps to be able to share a
proper package on GitHub:

- Start versionning with git if not already

- Connect to your GitHub account

- Create a minimal DESCRIPTION file if missing

  - You will have to update its content with your information after
    deployment

- Add NEWS file to present modifications of your releases

- Add README.Rmd and knit it to README.md to quickly present the aim and
  the use of your package

- Init continuous integration (CI)

  - Check the package on Linux, Windows and MacOS

  - Calculate code coverage. Note that you may need to connect to
    <https://about.codecov.io/> to see the results of the code coverage.

- Init continuous deployment (CD) of the 'pkgdown' website documentation

- Commit and push to GitHub

- List remaining manual steps to make the website public

Read
[`vignette("share-on-a-github-website", package = "fusen")`](https://thinkr-open.github.io/fusen/articles/share-on-a-github-website.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# This modifies the current directory and send it on GitHub
init_share_on_github()
} # }
```
