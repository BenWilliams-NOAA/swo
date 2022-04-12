library(devtools)
library(usethis)

use_description()
use_pipe()
use_package('data.table', type = 'depends')
use_package('tidytable')
use_package('purrr')
use_package('dplyr')
use_package('vroom')
use_package('here')
use_package('DBI')
use_package('odbc')

use_readme_md()
use_mit_license()

document()

use_r("utils")
use_r('query_data')
use_r('samples')
use_r('swo')

devtools::build()
