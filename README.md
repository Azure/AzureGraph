# AzureGraph

A simple interface to the [Azure Active Directory Graph API](https://docs.microsoft.com/en-au/azure/active-directory/develop/active-directory-graph-api). The companion package to [AzureRMR](https://github.com/cloudyr/AzureRMR) and [AzureAuth](https://github.com/cloudyr/AzureAuth).


## Authentication

AzureGraph uses the same authentication procedure as AzureRMR and the [Azure CLI](https://docs.microsoft.com/en-us/cli/azure/?view=azure-cli-latest). The first time you authenticate with a given Azure Active Directory tenant, you call `create_graph_login()` and supply your credentials. AzureGraph will prompt you for permission to create a special data directory in which to cache the obtained authentication token and AD Graph login. Once this information is saved on your machine, it can be retrieved in subsequent R sessions with `get_graph_login()`. Your credentials will be automatically refreshed so you don't have to reauthenticate.

**Linux DSVM note** If you are using a Linux Data Science Virtual Machine in Azure, you may have problems running `create_graph_login()` (ie, without arguments). In this case, try `create_graph_login(auth_type="device_code")`.


## Sample workflow

AzureGraph currently includes methods for creating and deleting registered apps and service principals.

```r
library(AzureGraph)

# authenticate with AAD
# - on first login, call create_graph_login()
# - on subsequent logins, call get_graph_login()
gr <- create_graph_login()

# create an app
# by default, this will have a randomly generated strong password with duration 1 year
app <- gr$create_app("AzureR_newapp")

# get the associated service principal
svc <- app$get_service_principal()
```

---
[![cloudyr project logo](https://i.imgur.com/JHS98Y7.png)](https://github.com/cloudyr)
