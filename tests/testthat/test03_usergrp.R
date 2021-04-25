context("Users/groups")

tenant <- Sys.getenv("AZ_TEST_TENANT_ID")
user <- Sys.getenv("AZ_TEST_USERPRINCIPALNAME")
admin_user <- Sys.getenv("AZ_TEST_ADMINUSERPRINCIPALNAME")

if(tenant == "" || user == "")
    skip("User method tests skipped: login credentials not set")

if(!interactive())
    skip("User method tests skipped: must be in interactive session")

scopes <- c("https://graph.microsoft.com/.default", "openid", "offline_access")
token <- AzureAuth::get_azure_token(scopes, tenant, .az_cli_app_id, version=2)
gr <- ms_graph$new(token=token)


test_that("User/group read functionality works",
{
    me <- gr$get_user()
    expect_equal(me$properties$userPrincipalName, admin_user)

    me2 <- gr$get_user(user)
    expect_equal(me2$properties$userPrincipalName, user)

    email <- me2$properties$mail
    me3 <- gr$get_user(email=email)
    expect_equal(me3$properties$userPrincipalName, user)

    name <- me2$properties$displayName
    me4 <- gr$get_user(name=name)
    expect_equal(me4$properties$userPrincipalName, user)

    users <- gr$list_users()
    expect_true(is.list(users) && all(sapply(users, is_user)))

    objs <- me$list_object_memberships()
    expect_true(is.character(objs))

    grps1 <- me$list_group_memberships()
    expect_true(is.character(grps1))

    grps3 <- me$list_direct_memberships()
    expect_true(all(sapply(grps3, function(x) is_group(x) || is_directory_role(x))))
    expect_true(all(sapply(grps3, function(g) !is.null(g$properties$id))))

    grp <- gr$get_group(grps1[1])
    expect_true(is_group(grp) && !is.null(grp$properties$id))

    grps <- gr$list_groups()
    expect_true(is.list(grps) && all(sapply(grps, is_group)))

    owned <- me$list_owned_objects()
    expect_true(is.list(owned) && all(sapply(owned, inherits, "az_object")))

    owned_apps <- me$list_owned_objects(type="application")
    expect_true(is.list(owned_apps) && all(sapply(owned_apps, is_app)))

    created <- me$list_created_objects()
    expect_true(is.list(created) && all(sapply(owned, inherits, "az_object")))

    created_apps <- me$list_created_objects(type="application")
    expect_true(is.list(created_apps) && all(sapply(created_apps, is_app)))
})


