
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cv

<!-- badges: start -->

![Github
Pages](https://github.com/goldmatthew/cv/actions/workflows/pages/pages-build-deployment/badge.svg?branch=master)

<!-- badges: end -->

This repository contains the files to create my academic CV using R
Markdown. The project is completely inspired by the
[`datadrivencv`](https://github.com/nstrayer/datadrivencv) package using
the `pagedown` package with the `html_resume` template. The CV is built
using some custom functions that parse an online Google Sheets
[file](https://docs.google.com/spreadsheets/d/1pN3UQrkQePh-I__yMG0RNbiSCi8Fcml39NUyPR-ODxc/edit?usp=drive_web&ouid=102131613587360182538).
To update my publications I use Paperpile and a custom Github action for
keeping updated a `bib` file.

## Files

-   `style/`: contains the default `resume.css` file with some extra
    tweaks
-   `docs/`: contains the `html` and `pdf` versions of the CV
-   `R/`: contains all the custom functions in order to create the CV
-   `data/`: contains the downloaded Google Sheet file and the `bib`
    file
