#' @export
az_app <- R6::R6Class("az_app",

public=list(

    token=NULL,
    tenant=NULL,

    # app data from server
    properties=NULL,

    initialize=function(token, tenant=NULL, app_id=NULL, object_id=NULL, password=NULL, password_duration=1, ...,
                        deployed_properties=list())
    {
        self$token <- token
        self$tenant <- tenant

        self$properties <- if(!is_empty(list(...)))
            private$init_and_deploy(..., password=password, password_duration=password_duration)
        else if(!is_empty(deployed_properties))
            private$init_from_parms(deployed_properties)
        else private$init_from_host(app_id, object_id)
    },

    delete=function(confirm=TRUE)
    {
        if(confirm && interactive())
        {
            msg <- paste0("Do you really want to delete the '", self$properties$displayName, "' app? (y/N) ")
            yn <- readline(msg)
            if(tolower(substr(yn, 1, 1)) != "y")
                return(invisible(NULL))
        }

        op <- file.path("applications", self$properties$objectId)
        call_graph_endpoint(self$token, self$tenant, op, http_verb="DELETE")
        invisible(NULL)
    },

    update=function(...)
    {},

    sync_fields=function()
    {
        self$properties <- private$init_from_host(app_id=NULL, object_id=self$properties$objectId)
        invisible(self)
    },

    create_service_principal=function(...)
    {
        az_service_principal$new(self$token, self$tenant, app_id=self$properties$appId, ..., mode="create")
    },

    get_service_principal=function()
    {
        az_service_principal$new(self$token, self$tenant, app_id=self$properties$appId, mode="get")
    },

    delete_service_principal=function(confirm=TRUE)
    {
        az_service_principal$new(self$token, self$tenant, app_id=self$properties$appId,
            deployed_properties=list(NULL), mode="get")$
            delete(confirm=confirm)
    }
),

private=list(
    
    password=NULL,

    init_and_deploy=function(..., password, password_duration)
    {
        properties <- list(...)
        if(is.null(password) || password != FALSE)
        {
            key <- "awBlAHkAMQA=" # base64/UTF-16LE encoded "key1"
            if(is.null(password))
            {
                chars <- c(letters, LETTERS, 0:9, "~", "!", "@", "#", "$", "%", "&", "*", "(", ")", "-", "+")
                password <- paste0(sample(chars, 50, replace=TRUE), collapse="")
            }

            end_date <- if(is.finite(password_duration))
            {
                now <- as.POSIXlt(Sys.time())
                now$year <- now$year + password_duration
                format(as.POSIXct(now), "%Y-%m-%dT%H:%M:%SZ", tz="GMT")
            }
            else "2299-12-30T12:00:00Z"

            private$password <- password
            properties <- modifyList(properties, list(passwordCredentials=list(list(
                customKeyIdentifier=key,
                endDate=end_date,
                value=password
            ))))
        }

        call_graph_endpoint(self$token, self$tenant, "applications", body=properties, encode="json", http_verb="POST")
    },

    init_from_parms=function(parms)
    {
        parms
    },

    init_from_host=function(app_id, object_id)
    {
        op <- if(is.null(object_id))
            file.path("applicationsByAppId", app_id)
        else file.path("applications", object_id)

        call_graph_endpoint(self$token, self$tenant, op)
    }
))
