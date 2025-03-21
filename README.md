Aspen Guide
================
2024-06-20

The goal of the aspen package is to provide data QC, visualization, and
publication assistance for MOJN aspen data.

### Installation

If you already have a GitHub token for RStudio run the code below to
download the aspen package. If you haven’t generated a GitHub token for
RStudio, you can follow the guide
[here](https://frbcesab.github.io/rsetup/chapters/github-token.html) to
set it up and then run the following code to download the aspen package.

``` r
# install.packages("devtools")
devtools::install_github(repo = "nationalparkservice/mojn-aspen-rpackage")
```

### Metadata and AGOL Password

Before running any of the functions in the package you should check that
the data in you aspen AGOL database is filled out. Instructions for
filling out AGOL metadata can be found
\[here\](<https://github.com/nationalparkservice/imd-fetchagol-package>.

Before running the functions, it is also recommended that you save the
password to your AGOL headless account using the keyring package in R.
To add headless account information to the default keyring follow the
steps below. The code should only have to be run once.

``` r
# Run the function below. Change the username field to the username of your headless account and input the password when prompted
keyring::key_set(service = "AGOL", username = "USERNAME", prompt = "Password for headless account: ")

# To check the key was saved run code below, filling in the username to your headless account
keyring::key_get(service = "AGOL", username = "USERNAME")
```

### Functions

A short summary of what this package contains. For more information see
function documentation.

- loadAndWrangleMOJNAspen(): load MOJN aspen data from AGOL into R and
  perform some basic data wrangling
- loadAndWrangleUCBNAspen(): load UCBN aspen data from AGOL into R and
  perform some basic data wrangling
- writeAspen(): write aspen data and metadata to CSVs

In addition to these functions the aspen package has some quality
control functions, a quality control script, and a visualization script.
To learn more about the functions see package documentation. You can
find the scripts in the script folder of the [GitHub
repo](https://github.com/nationalparkservice/mojn-aspen-rpackage) or if
you cloned the GitHub repo to your desktop you can open the files from
there. Download, open, and then run the .qmd file to see the results.
