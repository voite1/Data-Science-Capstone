## ----ckan:attach_packages, warning=FALSE, message=FALSE------------------
library(ckanr)    # Access data from CKAN.
library(magrittr) # Use pipelines for data processing.
library(readr)    # Modern data reader: read_csv().
library(dplyr)    # Data wrangling: select().
library(jsonlite) # Manage JSON objects.
library(readxl)   # Read Excel spreadsheets.

## ----ckan:list_servers, out.lines=25-------------------------------------
servers()

## ----ckan:set_default_server---------------------------------------------
get_default_url()
Sys.setenv(CKANR_DEFAULT_URL="http://data.gov.au")
get_default_url()

## ----ckan:get_info, out.lines=NULL---------------------------------------
ckan_info()

## ----ckan:list_organisaitons---------------------------------------------
organization_list(as="json") %>%
    fromJSON(flatten=TRUE) %>%
    extract2('result') ->
    orgs

## ----ckan:org_dimensions-------------------------------------------------
nrow(orgs)
names(orgs)

## ----ckan:list_orgs_json, out.lines=42-----------------------------------
orgs %>% toJSON() %>% prettify()

## ----ckan:find_ato_row---------------------------------------------------
orgs %>% 
  extract("title") %>%
  '=='("Australian Taxation Office") %>%
  which() %T>%
  print() ->
  ato

## ----ckanr:ato_package_count---------------------------------------------
orgs[ato,'packages']

## ----ckan:ato_org_id-----------------------------------------------------
oid.ato <- orgs[ato,'id'] %T>% print()

## ----ckan:list_specific_orgs_packages, out.lines=NULL--------------------
orgs %>%
  '['(seq(ato-5, ato+5),) %>%
  select(name, packages)

## ----ckan:package_list, out.lines=30-------------------------------------
package_list(as="table")
package_list(as="table", limit=NULL) %>% length()

## ----ckan:package_search_ato---------------------------------------------
package_search(q="australiantaxationoffice", as="json") %>%
    fromJSON(flatten=TRUE) %>%
    extract2('result') %>%
    extract2('results') ->
    pkgs.ato
names(pkgs.ato)
nrow(pkgs.ato)

## ----ckan:ato_pacakge_titles, out.lines=NULL-----------------------------
pkgs.ato$title

pkgs.ato$title %>%
  grep('Transparency', .) %>%
  extract(pkgs.ato$id, .) %T>%
  print() ->
  pid.ato.trans

## ----ckan:show_package---------------------------------------------------
pkg.ato.trans <- package_show(pid.ato.trans, as="table") %>% print()

## ----ckan:package_meta_data_list-----------------------------------------
names(pkg.ato.trans)
pkg.ato.trans$name
pkg.ato.trans$metadata_created
pkg.ato.trans$license_title
pkg.ato.trans$license_id
pkg.ato.trans$type
pkg.ato.trans$num_resources

## ----ckan:show:column----------------------------------------------------
names(pkg.ato.trans$resources)
pkg.ato.trans$resources[1,]$name
pkg.ato.trans$resources[1,]$format
pkg.ato.trans$resources[1,]$url

## ----ckan:download_ato_trans, eval=FALSE---------------------------------
## url <- pkg.ato.trans$resources[1,]$url
## temp  <- tempfile(fileext=".xlsx")
## download.file(url, temp)
## transparency <- read_excel(temp, sheet=2, skip=1)[1:1540, 2:6]
## unlink(temp)

## ----ckan:view_browser_trans, out.lines=10-------------------------------
dim(transparency)
names(transparency)
head(transparency)

## ------------------------------------------------------------------------
ds <- transparency
names(ds) <- c("name", "abn", "total", "taxable", "payable")
ds %<>% mutate(per=100*payable/total, rate=100*payable/taxable)

## ----ckan:list_groups----------------------------------------------------
group_list()

## ----ckan:list_transport_group-------------------------------------------
group_list() %T>% 
{
  sapply(., function(x) x$name=="transport") %>% 
  which() %>%
  '<<-'('i', .)
} %>%
  '[['(i) ->
  transport
transport
gid <- transport$id
gid

## ----ckan:group_characteristics------------------------------------------
transport$package_count
transport$approval_status

## ----ckan:show_group-----------------------------------------------------
group_show(gid)

## ----ckan:search_for_resource--------------------------------------------
resource_search(q = 'name:browser', limit = 2, as='table')

## ----ckan:save_package_id------------------------------------------------
resource_search(q = 'name:browser', limit = 2, as='table') $
  results %>%
  '[['('package_id') ->
  pid

## ----ckan:org_packages_resources-----------------------------------------
orgs %>% 
  extract("name") %>%
  '=='("digital-transformation-office") %>%
  which() %T>%
  print() ->
  dto

orgs %>%
  '['(seq(dto-5, dto+5),) %>%
  select(name, packages)

oid <- orgs[dto,'id']

oid

organization_show(id=oid, include_datasets=TRUE) %>%
  extract('packages') %>%
  sapply(function(x) x$id) %>% 
  sapply(package_show)

## ----ckan:qld------------------------------------------------------------
Sys.setenv(CKANR_DEFAULT_URL="https://data.qld.gov.au")
get_default_url()

qorgs <- organization_list(as="table") %T>% print()

## ----ckan:qld_edu--------------------------------------------------------
qorgs %>% 
  extract("name") %>%
  '=='("education-and-training") %>%
  which() %T>%
  print() ->
  qedu

## ----ckan:qld_edu_packages, eval=FALSE-----------------------------------
## qorgs.edu <- organization_show(id=qorgs[qedu,]$id, include_datasets=TRUE)

## ----ckan:qld_pretty-----------------------------------------------------
prettify(toJSON(qorgs.edu))

## ------------------------------------------------------------------------
qorgs.edu$result$packages %>%
  sapply(function(x) x$id) %T>%
  print() ->
  qorgs.edu.pid

sapply(package_show)

## ----eval=FALSE, echo=FALSE----------------------------------------------
## 
## 
## curorg <- organization_show(id='6203164d-caca-4ee8-a248-77e1a2809302',include_datasets = TRUE)
## 
## qld_edu_packages <- curorg$packages %>% sapply(function(x) x$id) %>% sapply(package_show)
## 
## ​
## 
## ss_qep <- qld_edu_packages[1:5] #just grab a subset to simplify
## 
## ss_qep_rs <- sapply(ss_qep, function(x) x$resources)
## 
## 	
## 
## ###Can just start straight with orgs in the following (and have code for that, but for speed just throwing in for packagse and resources
## 
## for(x in 1:length(ss_qep))
## 
## {
## 
##   print(paste('package id:',ss_qep[[x]]$id))
## 
##   print(paste('package name:',ss_qep[[x]]$name))
## 
##   print(paste('license title:',ss_qep[[x]]$license_title))
## 
##   print(paste('author_email:',ss_qep[[x]]$author_email))
## 
##   print(paste('organisation name:',ss_qep[[x]]$organization$name))
## 
##   print(paste('organisation title:',ss_qep[[x]]$organization$title))
## 
##   print(paste('organisation id',ss_qep[[x]]$organization$id,sep=":"))
## 
##   print(paste('package notes:',ss_qep[[x]]$notes))
## 
## 
## 
##   for (y in 1:length(ss_qep[[x]]$resources))
## 
##   {
## 
##     print(paste('resource id:',ss_qep[[x]]$resources[[y]]$id))
## 
##     print(paste('resource Used in data-driven application:',ss_qep[[x]]$resources[[y]]$'Used in data-driven application'))
## 
##     print(paste('resource datastore active:',ss_qep[[x]]$resources[[y]]$datastore_active))
## 
##     print(paste('resource Expiration date:',ss_qep[[x]]$resources[[y]]$'Expiration date'))
## 
##     print(paste('resource created:',ss_qep[[1]]$resources[[2]]$created))
## 
##     print(paste('resource last modified:',ss_qep[[1]]$resources[[2]]$last_modified))
## 
##     print(paste('resource revision timestamp:',ss_qep[[1]]$resources[[2]]$revision_timestamp))
## 
##     print(paste('resource webstore last updated:',ss_qep[[1]]$resources[[2]]$webstore_last_updated))
## 
##     print(paste('resource description:',ss_qep[[1]]$resources[[2]]$description))
## 
##     print(paste('resource format:',ss_qep[[1]]$resources[[2]]$format))
## 
##     print(paste('resource size:',ss_qep[[1]]$resources[[2]]$size))
## 
##     print(paste('resourece url:',ss_qep[[1]]$resources[[2]]$url))
## 
##     print(paste('resource revision id:',ss_qep[[1]]$resources[[2]]$revision_id))
## 
##     print(paste('resource name:',ss_qep[[1]]$resources[[2]]$name))
## 
##     print(paste('resource tracking summary total:',ss_qep[[1]]$resources[[2]]$tracking_summary$total))
## 
##     print(paste('resource tracking summary recent:',ss_qep[[1]]$resources[[2]]$tracking_summary$recent))
## 
##   }
## 
## 
## 
##   print('')
## 
## }

## ----eval=FALSE----------------------------------------------------------
## 
## 
## #Grab the data in JSON with the included datasets and flatten using fromJSON to convert nested structures into data frames where possible
## 
## orgs.json <- fromJSON(organization_show(id='6203164d-caca-4ee8-a248-77e1a2809302',include_datasets = TRUE,as="json"),flatten=TRUE)
## 
## ​
## 
## #Grab the organisation data
## 
## orgs <- orgs.json$result
## 
## ​
## 
## #Grab the packages data frame wth just the columns we need
## 
## orgs.pckgs <- select(orgs$packages
## 
##                      , package_id=id
## 
##                      , package_name=name
## 
##                      , package_license_title=license_title
## 
##                      , package_author_email=author_email
## 
##                      , package_organization_name=organization.name
## 
##                      , package_organization_title=organization.title
## 
##                      , package_organization_id=organization.id
## 
##                      , package_revision_timestamp=revision_timestamp
## 
##                      , package_notes=notes
## 
##                      , resources)
## 
## ​
## 
## #Loop through the packages so that we can then add a package id to each resource. We'll need this to join on.
## 
## for (i in 1:nrow(orgs.pckgs))
## 
## {
## 
##   #Add the package id to the related resource data frames and select just the columns we'll need
## 
##   orgs.pckgs$resources[[i]]<- select(mutate(orgs.pckgs$resources[[i]]
## 
##                                             , package_id = orgs.pckgs$package_id[[i]])
## 
##                                      , package_id
## 
##                                      , resource_id=id
## 
##                                      #, resource_used_in_data_driven_application=`Used in data-driven application` #Backticks seems to be failing here - we could rename first, but that seems inefficient
## 
##                                      #, resource_datastore_active=datastore_active #not sure why this is missing - was in the resource vector when using print statements in earlier code
## 
##                                      #, resource_expiration_date=`Expiration date` #Backticks seems to be failing here - we could rename first, but that seems inefficient
## 
##                                      , resource_created=created
## 
##                                      , resource_last_modified=last_modified
## 
##                                      , resource_revision_timestamp=revision_timestamp
## 
##                                      , resource_webstore_last_updated=webstore_last_updated
## 
##                                      , resource_description=description
## 
##                                      , resource_format=format
## 
##                                      , resource_size=size
## 
##                                      , resource_url=url
## 
##                                      , resource_revision_id=revision_id
## 
##                                      , resource_name=name
## 
##                                      , resource_tracking_summary_total=tracking_summary.total
## 
##                                      , resource_tracking_summary_recent=tracking_summary.recent)
## 
## }
## 
## ​
## 
## ​
## 
## #Combine the list of resource data frames within the packages data frame into one resource data frame, filling nulls with NAs
## 
## orgs.rsrcs <- rbind.fill(orgs.pckgs$resources)
## 
## ​
## 
## #Join the package and resource data frames based on the package id
## 
## orgs.df <- inner_join(orgs.pckgs, orgs.rsrcs)
## orgs.df
## 
## 

