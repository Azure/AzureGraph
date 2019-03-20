context("Users/groups")

tenant <- Sys.getenv("AZ_TEST_TENANT_ID")
user <- Sys.getenv("AZ_TEST_USERNAME")

if(tenant == "" || user == "")
    skip("User method tests skipped: login credentials not set")

if(!interactive())
    skip("User method tests skipped: must be in interactive session")

gr <- get_graph_login(tenant=tenant)


test_that("User/group read functionality works",
{
    me <- gr$get_user()
    expect_equal(me$properties$mail, user)

    me2 <- gr$get_user(user)
    expect_equal(me2$properties$mail, user)

    grps0 <- me$list_group_memberships(TRUE, TRUE)
    expect_true(is.character(grps0))

    grps1 <- me$list_group_memberships(TRUE, FALSE)
    expect_true(all(sapply(grps1, is_group)))
    expect_true(all(sapply(grps1, function(g) !is.null(g$properties$id))))

    grps2 <- me$list_group_memberships(FALSE, TRUE)
    expect_true(is.character(grps2))

    grps3 <- me$list_group_memberships(FALSE, FALSE)
    expect_true(all(sapply(grps3, is_group)))
    expect_true(all(sapply(grps3, function(g) !is.null(g$properties$id))))

    grp <- gr$get_group(grps0[1])
    expect_true(is_group(grp) && !is.null(grp$properties$id))
})


