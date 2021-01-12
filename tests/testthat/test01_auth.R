context("Authentication")

tenant <- Sys.getenv("AZ_TEST_TENANT_ID")
app <- Sys.getenv("AZ_TEST_APP_ID")
password <- Sys.getenv("AZ_TEST_PASSWORD")

if(tenant == "" || app == "" || password == "")
    skip("Authentication tests skipped: Microsoft Graph credentials not set")

if(!interactive())
    skip("Authentication tests skipped: must be in interactive session")

scopes <- c("https://graph.microsoft.com/.default", "openid", "offline_access")

clean_token_directory(confirm=FALSE)
suppressWarnings(file.remove(file.path(AzureR_dir(), "graph_logins.json")))

test_that("Graph authentication works",
{
    gr <- ms_graph$new(tenant=tenant, app=app, password=password)
    expect_is(gr, "ms_graph")
    expect_true(is_azure_token(gr$token))

    token <- get_azure_token(scopes, tenant, app=app, password=password, version=2)

    gr2 <- ms_graph$new(token=token)
    expect_is(gr2, "ms_graph")
    expect_true(is_azure_token(gr2$token))
})

test_that("Login interface works",
{
    delete_graph_login(tenant, confirm=FALSE)

    lst <- list_graph_logins()
    expect_true(is.list(lst))

    gr0 <- create_graph_login(tenant=tenant, app=app, password=password)
    expect_is(gr0, "ms_graph")
    expect_true(is_azure_v2_token(gr0$token))

    creds <- tempfile(fileext=".json")
    writeLines(jsonlite::toJSON(list(tenant=tenant, app=app, password=password)), creds)

    gr1 <- create_graph_login(config_file=creds)
    expect_identical(normalize_tenant(tenant), gr1$tenant)

    expect_length(list_graph_logins()[[normalize_tenant(tenant)]], 1)

    gr2 <- create_graph_login(tenant=tenant)
    expect_identical(gr2$token$client$client_id, .az_cli_app_id)

    expect_length(list_graph_logins()[[normalize_tenant(tenant)]], 2)

    gr3 <- create_graph_login(tenant=tenant, version=1)
    expect_identical(gr2$token$client$client_id, .az_cli_app_id)
    expect_true(is_azure_v1_token(gr3$token))

    expect_length(list_graph_logins()[[normalize_tenant(tenant)]], 3)

    gr4 <- create_graph_login(tenant=tenant, app=.az_cli_app_id, scopes="user.readwrite.all")

    expect_length(list_graph_logins()[[normalize_tenant(tenant)]], 4)

    gr5 <- get_graph_login(tenant, app=app)
    expect_identical(gr5$token$client$client_id, app)

    gr6 <- get_graph_login(tenant, scopes="user.readwrite.all")
    expect_identical(gr6$token$client$client_id, .az_cli_app_id)

    gr7 <- get_graph_login(tenant, scopes=NA)
    expect_true(is_azure_v1_token(gr7$token))

    gr8 <- get_graph_login(tenant, auth_type="client_credentials")
    expect_identical(gr8$token$client$client_id, app)
})

