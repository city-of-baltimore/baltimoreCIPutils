
<!-- README.md is generated from README.Rmd. Please edit that file -->

# baltimoreCIPutils

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

## Purpose

baltimoreCIPutils is an R package with utility functions designed to
support data import, tidying, and transformation using Workday and
Workday Adaptive Planning data sources for the Baltimore City Capital
Improvment Program (CIP) team in the Baltimore City Department of
Planning.

## Installation

You can install the development version of baltimoreCIPutils like so:

``` r
# pak::pkg_install("city-of-baltimore/baltimoreCIPutils")
```

## Example

The package includes several reference data sets:

``` r
library(baltimoreCIPutils)
## basic example code
```

``` r
wd_proj_hierarchy_xwalk
#>                                                 source         id
#> 1  Adaptive Planning CIP Reports - Project Hierarchy 2 PJHCIP9510
#> 2  Adaptive Planning CIP Reports - Project Hierarchy 2    PJH0734
#> 3  Adaptive Planning CIP Reports - Project Hierarchy 1    PJH1200
#> 4  Adaptive Planning CIP Reports - Project Hierarchy 2    PJH6700
#> 5  Adaptive Planning CIP Reports - Project Hierarchy 2 PJHCIP0539
#> 6  Adaptive Planning CIP Reports - Project Hierarchy 1    PJH2300
#> 7  Adaptive Planning CIP Reports - Project Hierarchy 2 PJHCIP0146
#> 8  Adaptive Planning CIP Reports - Project Hierarchy 1    PJH2500
#> 9  Adaptive Planning CIP Reports - Project Hierarchy 1    PJH2600
#> 10 Adaptive Planning CIP Reports - Project Hierarchy 2 PJHCIP0600
#> 11 Adaptive Planning CIP Reports - Project Hierarchy 2 PJHCIP0197
#> 12 Adaptive Planning CIP Reports - Project Hierarchy 1    PJH3100
#> 13 Adaptive Planning CIP Reports - Project Hierarchy 2 PJHCIP0588
#> 14 Adaptive Planning CIP Reports - Project Hierarchy 1    PJH7000
#> 15 Adaptive Planning CIP Reports - Project Hierarchy 2 PJHCIP0514
#> 16 Adaptive Planning CIP Reports - Project Hierarchy 1    PJH2700
#> 17 Adaptive Planning CIP Reports - Project Hierarchy 2 PJHCIP0312
#> 18 Adaptive Planning CIP Reports - Project Hierarchy 1    PJH3900
#> 19 Adaptive Planning CIP Reports - Project Hierarchy 1    PJH6300
#> 20 Adaptive Planning CIP Reports - Project Hierarchy 2 PJHCIP0610
#> 21 Adaptive Planning CIP Reports - Project Hierarchy 1    PJH4392
#> 22 Adaptive Planning CIP Reports - Project Hierarchy 2 PJHCIP0601
#> 23 Adaptive Planning CIP Reports - Project Hierarchy 1    PJH4301
#> 24 Adaptive Planning CIP Reports - Project Hierarchy 2 PJHCIP0593
#> 25 Adaptive Planning CIP Reports - Project Hierarchy 2    CIP0586
#> 26 Adaptive Planning CIP Reports - Project Hierarchy 1    PJH4303
#> 27 Adaptive Planning CIP Reports - Project Hierarchy 1    PJH4361
#> 28 Adaptive Planning CIP Reports - Project Hierarchy 2 PJHCIP0457
#> 29 Adaptive Planning CIP Reports - Project Hierarchy 1    PJH4371
#> 30 Adaptive Planning CIP Reports - Project Hierarchy 2 PJHCIP0474
#> 31 Adaptive Planning CIP Reports - Project Hierarchy 2 PJHCIP0110
#> 32 Adaptive Planning CIP Reports - Project Hierarchy 1    PJH5700
#> 33 Adaptive Planning CIP Reports - Project Hierarchy 2 PJHCIP0127
#> 34 Adaptive Planning CIP Reports - Project Hierarchy 1    PJH5900
#> 35 Adaptive Planning CIP Reports - Project Hierarchy 2 PJHCIP0116
#> 36 Adaptive Planning CIP Reports - Project Hierarchy 1    PJH6100
#> 37 Adaptive Planning CIP Reports - Project Hierarchy 2 PJHCIP0534
#> 38 Adaptive Planning CIP Reports - Project Hierarchy 2    PJH4361
#> 39 Adaptive Planning CIP Reports - Project Hierarchy 2 PJHCIP0417
#> 40 Adaptive Planning CIP Reports - Project Hierarchy 2 PJHCIP0418
#> 41 Adaptive Planning CIP Reports - Project Hierarchy 2 PJHCIP0188
#> 42 Adaptive Planning CIP Reports - Project Hierarchy 2 PJHCIP0206
#> 43 Adaptive Planning CIP Reports - Project Hierarchy 2 PJHCIP0517
#> 44 Adaptive Planning CIP Reports - Project Hierarchy 2 PJHCIP0520
#> 45 Adaptive Planning CIP Reports - Project Hierarchy 2 PJHCIP0512
#> 46 Adaptive Planning CIP Reports - Project Hierarchy 2 PJHCIP0525
#> 47 Adaptive Planning CIP Reports - Project Hierarchy 2 PJHCIP0557
#> 48 Adaptive Planning CIP Reports - Project Hierarchy 2    PJH6100
#> 49 Adaptive Planning CIP Reports - Project Hierarchy 2 PJHCIP0506
#> 50 Adaptive Planning CIP Reports - Project Hierarchy 2 PJHCIP0551
#> 51 Adaptive Planning CIP Reports - Project Hierarchy 2 PJHCIP0504
#> 52 Adaptive Planning CIP Reports - Project Hierarchy 2 PJHCIP0508
#> 53 Adaptive Planning CIP Reports - Project Hierarchy 2 PJHCIP0510
#> 54 Adaptive Planning CIP Reports - Project Hierarchy 2 PJHCIP0527
#> 55 Adaptive Planning CIP Reports - Project Hierarchy 2 PJHCIP0562
#> 56 Adaptive Planning CIP Reports - Project Hierarchy 2 PJHCIP0577
#> 57 Adaptive Planning CIP Reports - Project Hierarchy 2 PJHCIP0700
#>                                                           name
#> 1                      9510 Capital Projects - Street Lighting
#> 2          Capital Projects Division - Design and Construction
#> 3                                                  Comptroller
#> 4                                                      Parking
#> 5                               Capital Projects - Comptroller
#> 6                                                      Finance
#> 7                                   Capital Projects - Finance
#> 8                                                         Fire
#> 9                                             General Services
#> 10                                     Capital Projects - Fire
#> 11                         Capital Projects - General Services
#> 12                           Housing and Community Development
#> 13        Capital Projects - Housing and Community Development
#> 14                                              Transportation
#> 15                       Capital Projects - Street Resurfacing
#> 16                                                      Health
#> 17                                   Capital Projects - Health
#> 18                                    Enoch Pratt Free Library
#> 19                                        Recreation and Parks
#> 20   Capital Projects - Housing and Community Development HOME
#> 21                                M-R American Rescue Plan Act
#> 22  Capital Projects - Baltimore Development Corporation (BDC)
#> 23                                                   Mayoralty
#> 24   Capital Projects - Housing and Community Development CDBG
#> 25 Capital Projects - Housing and Community Development Anchor
#> 26                     M-R: Office of Information & Technology
#> 27                                     M-R: Convention Complex
#> 28                 Capital Projects - Enoch Pratt Free Library
#> 29                           M-R Baltimore City Public Schools
#> 30                     Capital Projects - Recreation and Parks
#> 31        Capital Projects - American Recovery Plan Act (ARPA)
#> 32                                                    Planning
#> 33                        Capital Projects - Mayoralty-Related
#> 34                                                      Police
#> 35        Capital Projects - Information and Technology (BCIT)
#> 36                                                Public Works
#> 37                        Capital Projects - Convention Center
#> 38                                     M-R: Convention Complex
#> 39                   Capital Projects - City Schools Systemics
#> 40                Capital Projects - City Schools Construction
#> 41                                 Capital Projects - Planning
#> 42                                   Capital Projects - Police
#> 43                              Capital Projects - Solid Waste
#> 44                               Capital Projects - Stormwater
#> 45                      Capital Projects - Traffic Engineering
#> 46            Capital Projects - Pollution and Erosion Control
#> 47                             Capital Projects - Water Supply
#> 48                                                Public Works
#> 49                                  Capital Projects - Bridges
#> 50                               Capital Projects - Wastewater
#> 51                      Capital Projects - Alleys and Footways
#> 52                     Capital Projects - Streets and Highways
#> 53                          Capital Projects - Street Lighting
#> 54             Capital Projects - Development Agencies Program
#> 55                                 Capital Projects - Conduits
#> 56                        Capital Projects - Parking Authority
#> 57                           Capital Projects - Transportation
#>                                             entity    use
#> 1                        DOT Street Lighting Group Active
#> 2                    DGS Capital Projects Division Active
#> 3              Baltimore City Comptroller's Office Active
#> 4              Parking Authority of Baltimore City Active
#> 5              Baltimore City Comptroller's Office Active
#> 6                            Department of Finance Active
#> 7                            Department of Finance Active
#> 8                   Baltimore City Fire Department Active
#> 9                   Department of General Services Active
#> 10                  Baltimore City Fire Department Active
#> 11                  Department of General Services Active
#> 12 Department of Housing and Community Development Active
#> 13 Department of Housing and Community Development Active
#> 14                    Department of Transportation Active
#> 15      DOT Highways and Streets Engineering Group Active
#> 16                               Health Department Active
#> 17                               Health Department Active
#> 18                        Enoch Pratt Free Library Active
#> 19              Department of Recreation and Parks Active
#> 20 Department of Housing and Community Development Active
#> 21                                  Baltimore City Active
#> 22               Baltimore Development Corporation Active
#> 23                   Baltimore City Mayor's Office Active
#> 24 Department of Housing and Community Development Active
#> 25 Department of Housing and Community Development Active
#> 26           Baltimore City Information Technology Active
#> 27                     Baltimore Convention Center Active
#> 28                        Enoch Pratt Free Library Active
#> 29                   Baltimore City Public Schools Active
#> 30              Department of Recreation and Parks Active
#> 31             Mayor's Office of Recovery Programs Active
#> 32                          Department of Planning Active
#> 33                   Baltimore City Mayor's Office Active
#> 34                Baltimore City Police Department Active
#> 35           Baltimore City Information Technology Active
#> 36                      Department of Public Works Active
#> 37                     Baltimore Convention Center Active
#> 38                     Baltimore Convention Center Active
#> 39                   Baltimore City Public Schools Active
#> 40                   Baltimore City Public Schools Active
#> 41                          Department of Planning Active
#> 42                Baltimore City Police Department Active
#> 43                       DPW Bureau of Solid Waste Active
#> 44                         DPW Stormwater Division Active
#> 45                            DOT Traffic Division Active
#> 46                         DPW Stormwater Division Active
#> 47                              DPW Water Division Active
#> 48                      Department of Public Works Active
#> 49       DOT Engineering and Construction Division Active
#> 50                         DPW Wastewater Division Active
#> 51            DOT Footways Group, DOT Alleys Group Active
#> 52      DOT Highways and Streets Engineering Group Active
#> 53                       DOT Street Lighting Group Active
#> 54                DOT Development Agencies Program Active
#> 55                            DOT Conduit Division Active
#> 56             Parking Authority of Baltimore City Active
#> 57                    Department of Transportation Active
#>                                                                                                                                                                                                notes
#> 1                                                                                                                                                                        Added later - missing order
#> 2                                                                                                                                                                        Added later - missing order
#> 3                                                                                                                                                                                               <NA>
#> 4                                                                                                                                                                                               <NA>
#> 5                                                                                                                                                                                               <NA>
#> 6                                                                                                                                                                                               <NA>
#> 7                                                                                                                                                                                               <NA>
#> 8                                                                                                                                                                                               <NA>
#> 9                                                                                                                                                                                               <NA>
#> 10                                                                                                                                                                                              <NA>
#> 11                                                                                                                                                                                              <NA>
#> 12                                                                                                                                                                                              <NA>
#> 13                                                                                                                                                                                              <NA>
#> 14                                                                                                                                                                                              <NA>
#> 15                                                                                                                                                                                              <NA>
#> 16                                                                                                                                                                                              <NA>
#> 17                                                                                                                                                                                              <NA>
#> 18                                                                                                                                                                                              <NA>
#> 19                                                                                                                                                                                              <NA>
#> 20                                                                                                                                                                                              <NA>
#> 21                                                                                                                                                                                              <NA>
#> 22                                                                                                                                                                                              <NA>
#> 23                                                                                                                                                                                              <NA>
#> 24                                                                                                                                                                                              <NA>
#> 25                                                                                                                                                                                              <NA>
#> 26                                                                                                                                                                                              <NA>
#> 27                                                                                                                                                                                              <NA>
#> 28                                                                                                                                                                                              <NA>
#> 29                                                                                                                                                                                              <NA>
#> 30                                                                                                                                                                                              <NA>
#> 31                                                                                                                                                                                              <NA>
#> 32                                                                                                                                                                                              <NA>
#> 33                                                                                                                                                                                              <NA>
#> 34                                                                                                                                                                                              <NA>
#> 35                                                                                                                                                                                              <NA>
#> 36                                                                                                                                                                                              <NA>
#> 37                                                                                                                                                                                              <NA>
#> 38                                                                                                                                                                                              <NA>
#> 39                                                                                                                                                                                              <NA>
#> 40                                                                                                                                                                                              <NA>
#> 41                                                                                                                                                                                              <NA>
#> 42                                                                                                                                                                                              <NA>
#> 43                                                                                                                                                                                              <NA>
#> 44                                                                                                                                                                                              <NA>
#> 45                                                                                                                                                                                              <NA>
#> 46                                                                                                                                                                                              <NA>
#> 47                                                                                                                                                                                              <NA>
#> 48                                                                                                                                                                                              <NA>
#> 49                                                                                                                                                                                              <NA>
#> 50                                                                                                                                       Error in Workday for project name; expect to see this fixed
#> 51                                                                                                                                                                                              <NA>
#> 52                                                                                                                                                                                              <NA>
#> 53                                                                                                                                                                                              <NA>
#> 54 Projects completed by DOT typically on behalf of private developer; impetus of BDC, e.g. BDC negotiated with Stadium Square to get GO Bonds for capital improvements instead of private developer
#> 55                                                                                                                                                                                              <NA>
#> 56                                                                                                                                                                                              <NA>
#> 57                                                                                                                                                                                              <NA>
#>                 createdTime PHierarchy1 Code PHierarchy2 Code
#> 1  2023-11-21T17:41:39.000Z             <NA>       PJHCIP9510
#> 2  2023-11-21T17:42:11.000Z             <NA>          PJH0734
#> 3  2023-11-02T13:15:08.000Z          PJH1200             <NA>
#> 4  2023-11-02T13:15:08.000Z             <NA>          PJH6700
#> 5  2023-11-02T13:15:08.000Z             <NA>       PJHCIP0539
#> 6  2023-11-02T13:15:08.000Z          PJH2300             <NA>
#> 7  2023-11-02T13:15:08.000Z             <NA>       PJHCIP0146
#> 8  2023-11-02T13:15:08.000Z          PJH2500             <NA>
#> 9  2023-11-02T13:15:08.000Z          PJH2600             <NA>
#> 10 2023-11-02T13:15:08.000Z             <NA>       PJHCIP0600
#> 11 2023-11-02T13:15:08.000Z             <NA>       PJHCIP0197
#> 12 2023-11-02T13:15:08.000Z          PJH3100             <NA>
#> 13 2023-11-02T13:15:08.000Z             <NA>       PJHCIP0588
#> 14 2023-11-02T13:15:08.000Z          PJH7000             <NA>
#> 15 2023-11-02T13:15:08.000Z             <NA>       PJHCIP0514
#> 16 2023-11-02T13:15:08.000Z          PJH2700             <NA>
#> 17 2023-11-02T13:15:08.000Z             <NA>       PJHCIP0312
#> 18 2023-11-02T13:15:08.000Z          PJH3900             <NA>
#> 19 2023-11-02T13:15:08.000Z          PJH6300             <NA>
#> 20 2023-11-02T13:15:08.000Z             <NA>       PJHCIP0610
#> 21 2023-11-02T13:15:08.000Z          PJH4392             <NA>
#> 22 2023-11-02T13:15:08.000Z             <NA>       PJHCIP0601
#> 23 2023-11-02T13:15:08.000Z          PJH4301             <NA>
#> 24 2023-11-02T13:15:08.000Z             <NA>       PJHCIP0593
#> 25 2023-11-02T13:15:08.000Z             <NA>          CIP0586
#> 26 2023-11-02T13:15:08.000Z          PJH4303             <NA>
#> 27 2023-11-02T13:15:08.000Z          PJH4361             <NA>
#> 28 2023-11-02T13:15:08.000Z             <NA>       PJHCIP0457
#> 29 2023-11-02T13:15:08.000Z          PJH4371             <NA>
#> 30 2023-11-02T13:15:08.000Z             <NA>       PJHCIP0474
#> 31 2023-11-02T13:15:08.000Z             <NA>       PJHCIP0110
#> 32 2023-11-02T13:15:08.000Z          PJH5700             <NA>
#> 33 2023-11-02T13:15:08.000Z             <NA>       PJHCIP0127
#> 34 2023-11-02T13:15:08.000Z          PJH5900             <NA>
#> 35 2023-11-02T13:15:08.000Z             <NA>       PJHCIP0116
#> 36 2023-11-02T13:15:08.000Z          PJH6100             <NA>
#> 37 2023-11-02T13:15:08.000Z             <NA>       PJHCIP0534
#> 38 2023-11-02T13:15:08.000Z             <NA>          PJH4361
#> 39 2023-11-02T13:15:08.000Z             <NA>       PJHCIP0417
#> 40 2023-11-02T13:15:08.000Z             <NA>       PJHCIP0418
#> 41 2023-11-02T13:15:08.000Z             <NA>       PJHCIP0188
#> 42 2023-11-02T13:15:08.000Z             <NA>       PJHCIP0206
#> 43 2023-11-02T13:15:08.000Z             <NA>       PJHCIP0517
#> 44 2023-11-02T13:15:08.000Z             <NA>       PJHCIP0520
#> 45 2023-11-02T13:15:08.000Z             <NA>       PJHCIP0512
#> 46 2023-11-02T13:15:08.000Z             <NA>       PJHCIP0525
#> 47 2023-11-02T13:15:08.000Z             <NA>       PJHCIP0557
#> 48 2023-11-02T13:15:08.000Z             <NA>          PJH6100
#> 49 2023-11-02T13:15:08.000Z             <NA>       PJHCIP0506
#> 50 2023-11-02T13:15:08.000Z             <NA>       PJHCIP0551
#> 51 2023-11-02T13:15:08.000Z             <NA>       PJHCIP0504
#> 52 2023-11-02T13:15:08.000Z             <NA>       PJHCIP0508
#> 53 2023-11-02T13:15:08.000Z             <NA>       PJHCIP0510
#> 54 2023-11-02T13:15:08.000Z             <NA>       PJHCIP0527
#> 55 2023-11-02T13:15:08.000Z             <NA>       PJHCIP0562
#> 56 2023-11-02T13:15:08.000Z             <NA>       PJHCIP0577
#> 57 2023-11-02T13:15:08.000Z             <NA>       PJHCIP0700
```
