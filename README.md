Aspen Guide
================
2026-11-18

This package supports data collected under the multi-network aspen
monitoring protocol used by the Mojave Desert Network (MOJN) and the
Upper Columbia Basin Network (UCBN). It provides provides tools for
three distinct parts of the data life cycle: data publication, quality
control, and visualization. This package is still in development.

The aspen monitoring protocol and other documents can be accessed on NPS
DataStore: [Mojave Desert Network Aspen
Monitoring](https://irma.nps.gov/DataStore/Reference/Profile/2256754)
[Upper Columbia Basin Network Aspen
Monitoring](https://irma.nps.gov/DataStore/Reference/Profile/2318971)

## Installation

If you already have a GitHub token for RStudio run the code below to
download the aspen package. If you haven’t generated a GitHub token for
RStudio, you can follow the guide
[here](https://frbcesab.github.io/rsetup/chapters/github-token.html) to
set it up and then run the following code to download the aspen package.

``` r
# install.packages("devtools")
devtools::install_github(repo = "nationalparkservice/mojn-aspen-rpackage")
```

## AGOL Connection

To access the aspen data, you need AGOL credentials. It is recommended
that you use a headless account and that you save the password to your
AGOL headless account using the keyring package in R. To add account
information to the default keyring follow the steps below. The code
should only need to be run once.

It is also recommended that the metadata in your aspen AGOL database is
filled out and up to date. Instructions for filling out AGOL metadata
can be found
[here](https://github.com/nationalparkservice/imd-fetchagol-package).

``` r
# Run the function below. Change the username field to the username of your headless account and input the password when prompted
keyring::key_set(service = "AGOL", username = "USERNAME", prompt = "Password for headless account: ")

# To check the key was saved run code below, filling in the username to your headless account
keyring::key_get(service = "AGOL", username = "USERNAME")
```

## Functions

Examples of package functionality. For more information see function
documentation.

#### Data Publication

- `load_and_package_aspen()`: loads aspen data stored on AGOL and
  wrangles it into data package-formatted tables for publication
- `write_aspen()`: writes the results of `load_and_package_aspen()` to
  CSV files

#### Quality Control (QC)

- `check_site_visit()`: checks that each site visit has observations
  recorded; checks that each site visit has at least one live aspen
  recorded; checks for sites that were visited more than once in a
  calendar year
- `check_tree_id()`: checks for trees that have not been identified and
  whether any were alive
- `check_tree_count()`: checks for tree records in the Observations
  table that have a total count of zero or a total count greater than
  250
- `check_size_class()`: checks for trees that are missing a size class
  record; checks for trees with multiple records for a single size class

#### Visualization

In development.
