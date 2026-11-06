# import_cpd

import_cpd

## Usage

``` r
new_cpd(
  title = "",
  type = "other",
  date = today(),
  learning_hours = 0,
  path = "cpd-records",
  overwrite = FALSE
)
```

## Arguments

- title:

  Title for new CPD record

- type:

  Type of new CPD record, can be one of "work", "professional",
  "formal", "self" or "other"

- date:

  The start date of the activity in yyyy-mm-dd format

- learning_hours:

  The number of learning hours acruded during this activity

- path:

  Path to CPD records

- overwrite:

  Should the YAML be overwritten if it exists - default FALSE
