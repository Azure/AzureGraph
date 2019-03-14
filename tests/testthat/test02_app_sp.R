context("App creation/deletion")

tenant <- Sys.getenv("AZ_TEST_TENANT_ID")
appnative <- Sys.getenv("AZ_TEST_NATIVE_APP_ID")
subscription <- Sys.getenv("AZ_TEST_SUBSCRIPTION")

if(tenant == "" || appnative == "" || subscription == "")
    skip("App method tests skipped: ARM credentials not set")

if(!interactive())
    skip("App method tests skipped: must be in interactive session")

az <- get_graph_login(tenant=tenant, app=appnative)
sub <- az$get_subscription(subscription)

test_that("App creation works",
{
    newapp_name <- paste0("AzureRtest_", paste0(sample(letters, 5, TRUE), collapse=""))
    newapp <- az$create_app(name=newapp_name, create_service_principal=FALSE)
    newsvc <- newapp$create_service_principal()
    expect_true(is_app(newapp))
    expect_true(is_service_principal(newsvc))

    newapp_id <- newapp$properties$appId
    expect_true(is_app(az$get_app(app_id=newapp_id)))
    expect_true(is_service_principal(az$get_service_principal(app_id=newapp_id)))
    expect_true(is_service_principal(newapp$get_service_principal()))

    Sys.setenv(AZ_TEST_NEWAPP_ID=newapp_id)
})

test_that("App deletion works",
{
    newapp_id <- Sys.getenv("AZ_TEST_NEWAPP_ID")

    expect_silent(az$delete_service_principal(app_id=newapp_id, confirm=FALSE))
    expect_silent(az$delete_app(app_id=newapp_id, confirm=FALSE))

    Sys.sleep(2)
    expect_error(az$get_app(app_id=newapp_id))
})

sub$get_resource_group(Sys.getenv("AZ_TEST_NEWRG"))$delete(confirm=FALSE)
Sys.unsetenv("AZ_TEST_NEWAPP_ID")
Sys.unsetenv("AZ_TEST_NEWRG")
