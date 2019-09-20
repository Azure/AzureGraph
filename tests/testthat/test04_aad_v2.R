context("AAD v2.0")

tenant <- Sys.getenv("AZ_TEST_TENANT_ID")
app <- Sys.getenv("AZ_TEST_NATIVE_APP_ID")

if(tenant == "" || app == "")
    skip("AAD v2.0 tests skipped: Microsoft Graph credentials not set")

if(!interactive())
    skip("AAD v2.0 tests skipped: must be in interactive session")


test_that("Graph authentication works",
{
    token <- get_azure_token(c("https://graph.microsoft.com/.default", "offline_access"), tenant, app, version=2)

    gr <- ms_graph$new(token=token)
    expect_is(gr, "ms_graph")
    expect_true(is_azure_token(gr$token))

    me <- gr$get_user()
    expect_is(me, "az_user")
})
