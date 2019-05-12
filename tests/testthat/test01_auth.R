context("Authentication")

tenant <- Sys.getenv("AZ_TEST_TENANT_ID")
app <- Sys.getenv("AZ_TEST_NATIVE_APP_ID")

if(tenant == "" || app == "")
    skip("Authentication tests skipped: Microsoft Graph credentials not set")

if(!interactive())
    skip("Authentication tests skipped: must be in interactive session")


test_that("Graph authentication works",
{
    gr <- ms_graph$new(tenant=tenant, app=app)
    expect_is(gr, "ms_graph")
    expect_true(is_azure_token(gr$token))

    token <- get_azure_token("https://graph.microsoft.com/", tenant, app)
                        
    gr2 <- ms_graph$new(token=token)
    expect_is(gr2, "ms_graph")
    expect_true(is_azure_token(gr2$token))
})

test_that("Login interface works",
{
    lst <- list_graph_logins()
    expect_true(is.list(lst))

    gr3 <- create_graph_login(tenant=tenant, app=app)
    expect_is(gr3, "ms_graph")

    creds <- tempfile(fileext=".json")
    writeLines(jsonlite::toJSON(list(tenant=tenant, app=app)), creds)

    gr4 <- create_graph_login(config_file=creds)
    expect_is(gr4, "ms_graph")
    expect_identical(normalize_tenant(tenant), gr4$tenant)

    gr5 <- get_graph_login(tenant)
    expect_is(gr5, "ms_graph")
})

