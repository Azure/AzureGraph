#' Device in Azure Active Directory
#'
#' Class representing a registered device.
#'
#' @docType class
#' @section Fields:
#' - `token`: The token used to authenticate with the Graph host.
#' - `tenant`: The Azure Active Directory tenant for this group.
#' - `type`: always "device" for a device object.
#' - `properties`: The device properties.
#' @section Methods:
#' - `new(...)`: Initialize a new device object. Do not call this directly; see 'Initialization' below.
#' - `delete(confirm=TRUE)`: Delete a device. By default, ask for confirmation first.
#' - `update(...)`: Update the device information in Azure Active Directory.
#' - `do_operation(...)`: Carry out an arbitrary operation on the device.
#' - `sync_fields()`: Synchronise the R object with the app data in Azure Active Directory.
#'
#' @section Initialization:
#' Create objects of this class via the `list_registered_devices()` and `list_owned_devices()` methods of the `az_user` class.
#'
#' @seealso
#' [ms_graph], [az_user], [az_object]
#'
#' [Microsoft Graph overview](https://learn.microsoft.com/en-us/graph/overview),
#' [REST API reference](https://learn.microsoft.com/en-us/graph/api/overview?view=graph-rest-1.0)
#'
#' @format An R6 object of class `az_device`, inheriting from `az_object`.
#' @export
az_device <- R6::R6Class("az_device", inherit=az_object,

public=list(

    initialize=function(token, tenant=NULL, properties=NULL)
    {
        self$type <- "device"
        super$initialize(token, tenant, properties)
    },

    print=function(...)
    {
        cat("<Graph device '", self$properties$displayName, "'>\n", sep="")
        cat("  directory id:", self$properties$id, "\n")
        cat("  device id:", self$properties$deviceId, "\n")
        invisible(self)
    }
))
