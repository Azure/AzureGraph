context("Users/groups")

tenant <- Sys.getenv("AZ_TEST_TENANT_ID")
user <- Sys.getenv("AZ_TEST_USERPRINCIPALNAME")

if(tenant == "" || user == "")
    skip("User method tests skipped: login credentials not set")

if(!interactive())
    skip("User method tests skipped: must be in interactive session")

gr <- get_graph_login(tenant=tenant)


test_that("User/group read functionality works",
{
    me <- gr$get_user()
    expect_equal(me$properties$userPrincipalName, user)

    me2 <- gr$get_user(user)
    expect_equal(me2$properties$userPrincipalName, user)

    objs <- me$list_object_memberships()
    expect_true(is.character(objs))

    grps1 <- me$list_group_memberships()
    expect_true(is.character(grps1))

    grps2 <- me$list_direct_memberships(id_only=TRUE)
    expect_true(is.character(grps2))

    grps3 <- me$list_direct_memberships(id_only=FALSE)
    expect_true(all(sapply(grps3, is_group)))
    expect_true(all(sapply(grps3, function(g) !is.null(g$properties$id))))

    grp <- gr$get_group(grps1[1])
    expect_true(is_group(grp) && !is.null(grp$properties$id))

    owned <- me$list_owned_objects()
    expect_true(is.list(owned) && all(sapply(owned, inherits, "az_object")))

    owned_apps <- me$list_owned_objects(type="application")
    expect_true(is.list(owned_apps) && all(sapply(owned_apps, is_app)))

    created <- me$list_created_objects()
    expect_true(is.list(created) && all(sapply(owned, inherits, "az_object")))

    created_apps <- me$list_created_objects(type="application")
    expect_true(is.list(created_apps) && all(sapply(created_apps, is_app)))
})


