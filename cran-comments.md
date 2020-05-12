## Resubmission 2
This is a resubmission. In this version I have:
* Added single quotes around object model in Description, 'Reference Classes' are already 
quoted with undirected single quotes so this is the only other expression that is is not
commonly understood english that I could think of.
* Hide some methods that are internal only from being listed in the documentation using #' #'@keywords internal
* Update NEWS file with all version history

## Resubmission 2
This is a resubmission. In this version I have:
* Removed all print calls and replaced with warning or message when relevant
* Add Authors@R field instead if Author and Copyright fields as per feed-back.
* Add Steven Brandt as contributor as his article in Java World formed the basis of the XMl parser 
* Add \value tag to utils.Rd
* Add single quote in Title and Description fields where required in DESCRPTION 

## Resubmission 1
This is a resubmission. In this version I have:
* Fixed the LICENSE to adhere to the standard template format for MIT


## Initial submission
My first submission to CRAN

## Test environments
 Linux Mint 19.3, R 3.6.3
 Windows 10, R 3.6.1
 
 check_win_release(): R version 3.6.3 (2020-02-29)
 check_win_devel(): R Under development (unstable) (2020-05-01 r78341
 
 ## R CMD check results
 There were no ERRORs or WARNINGs 
 
 There was one NOTE:
* checking CRAN incoming feasibility ... NOTE
    Maintainer: 'Per Nyfelt <per@alipsa.se>'

    New submission
 
 As far as I can tell this is only a due to the fact that this is the first time I submitted something to CRAN and
 not something I can fix.
 
 ## Downstream dependencies
 None