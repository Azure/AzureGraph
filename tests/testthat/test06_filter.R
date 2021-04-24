context("List filtering")

tenant <- Sys.getenv("AZ_TEST_TENANT_ID")
user <- Sys.getenv("AZ_TEST_USERPRINCIPALNAME")

if(tenant == "" || user == "")
    skip("Paging tests skipped: login credentials not set")

if(!interactive())
    skip("Paging tests skipped: must be in interactive session")

scopes <- c("https://graph.microsoft.com/.default", "openid", "offline_access")
token <- AzureAuth::get_azure_token(scopes, tenant, .az_cli_app_id, version=2)
gr <- ms_graph$new(token=token)
me <- gr$get_user(user)


test_that("Filtering works",
{
    id <- me$list_group_memberships()[1]
    grp <- gr$get_aad_object(id)
    expect_true(inherits(grp, "az_group") && !is.null(grp$properties$displayName))
    filtexpr1 <- sprintf("displayName eq '%s'", grp$properties$displayName)

    expect_error(me$list_group_memberships(filter=filtexpr1))

    lst1 <- me$list_direct_memberships(filter=filtexpr1)
    expect_is(lst1, "list")
    expect_true(length(lst1) == 1 &&
                inherits(lst1[[1]], "az_group") &&
                lst1[[1]]$properties$displayName == grp$properties$displayName)

    filtexpr2 <- sprintf("userPrincipalName eq '%s'", user)
    lst2 <- grp$list_members(filter=filtexpr2)
    expect_is(lst2, "list")
    expect_true(length(lst2) == 1 &&
                inherits(lst2[[1]], "az_user") &&
                lst2[[1]]$properties$userPrincipalName == user)
})

