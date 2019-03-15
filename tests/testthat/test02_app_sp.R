context("App creation/deletion")

tenant <- Sys.getenv("AZ_TEST_TENANT_ID")

if(tenant == "")
    skip("App method tests skipped: login credentials not set")

if(!interactive())
    skip("App method tests skipped: must be in interactive session")

gr <- get_graph_login(tenant=tenant)


test_that("App creation works",
{
    newapp_name <- paste0("AzureRtest_", paste0(sample(letters, 5, TRUE), collapse=""))
    newapp <- gr$create_app(name=newapp_name, create_service_principal=FALSE)
    newsvc <- newapp$create_service_principal()
    expect_true(is_app(newapp))
    expect_true(is_service_principal(newsvc))

    newapp_id <- newapp$properties$appId
    expect_true(is_app(gr$get_app(app_id=newapp_id)))
    expect_true(is_service_principal(gr$get_service_principal(app_id=newapp_id)))
    expect_true(is_service_principal(newapp$get_service_principal()))

    expect_type(newapp$update_password(), "character")
    expect_true(is_app(newapp$update(displayName=paste0(newapp_name, "_update"))))

    Sys.setenv(AZ_TEST_NEWAPP_ID=newapp_id)
})

test_that("App deletion works",
{
    newapp_id <- Sys.getenv("AZ_TEST_NEWAPP_ID")

    expect_silent(gr$delete_service_principal(app_id=newapp_id, confirm=FALSE))
    expect_silent(gr$delete_app(app_id=newapp_id, confirm=FALSE))
})
