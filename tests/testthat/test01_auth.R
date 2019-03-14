context("Authentication")

tenant <- Sys.getenv("AZ_TEST_TENANT_ID")
app <- Sys.getenv("AZ_TEST_NATIVE_APP_ID")
subscription <- Sys.getenv("AZ_TEST_SUBSCRIPTION")

if(tenant == "" || app == "" || subscription == "")
    skip("Authentication tests skipped: AD Graph credentials not set")


test_that("Graph authentication works",
{
    az <- az_graph$new(tenant=tenant, app=app)
    expect_is(az, "az_graph")
    expect_true(is_azure_token(az$token))

    creds <- tempfile(fileext=".json")
    writeLines(jsonlite::toJSON(list(tenant=tenant, app=app)), creds)
                        
    az2 <- az_graph$new(config_file=creds)
    expect_is(az2, "az_graph")
    expect_true(is_azure_token(az2$token))
})

test_that("Login interface works",
{
    lst <- list_graph_logins()
    expect_true(is.list(lst))

    az3 <- create_graph_login(tenant=tenant, app=app)
    expect_is(az3, "az_graph")

    creds <- tempfile(fileext=".json")
    writeLines(jsonlite::toJSON(list(tenant=tenant, app=app)), creds)

    az4 <- create_graph_login(config_file=creds)
    expect_is(az4, "az_graph")

    az5 <- get_graph_login(tenant)
    expect_is(az5, "az_graph")
})

