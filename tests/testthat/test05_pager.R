context("List paging")

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


test_that("Paging works",
{
    lst <- me$do_operation("memberOf")
    expect_silent(p <- me$get_list_pager(lst, generate_objects=FALSE))
    expect_is(p, "ms_graph_pager")

    # assume returned list fits in 1 page
    out <- p$value
    expect_is(out, "list")
    expect_null(p$value)

    # return lists
    lst1 <- me$do_operation("memberOf", options=list(`$top`=1))
    p1 <- me$get_list_pager(lst1, generate_objects=FALSE)
    expect_is(p1, "ms_graph_pager")

    for(i in seq_along(out))
        expect_identical(out[i], p1$value)
    expect_null(p1$value)
})


test_that("Page result as data frame works",
{
    lstdf <- me$do_operation("memberOf", simplify=TRUE)
    expect_silent(pdf <- me$get_list_pager(lstdf))
    expect_is(pdf, "ms_graph_pager")

    outdf <- pdf$value
    expect_is(outdf, "data.frame")
    expect_null(pdf$value)

    # return data frames
    lstdf1 <- me$do_operation("memberOf", options=list(`$top`=1), simplify=TRUE)
    pdf1 <- me$get_list_pager(lstdf1)
    expect_is(pdf1, "ms_graph_pager")

    outdf1 <- list()
    for(i in seq_len(nrow(outdf)))
        outdf1 <- c(outdf1, list(pdf1$value))
    outdf1 <- do.call(vctrs::vec_rbind, outdf1)
    expect_identical(outdf, outdf1)
    expect_null(pdf1$value)
})


test_that("Page result as object works",
{
    lstobj <- me$do_operation("memberOf")
    expect_silent(pobj <- me$get_list_pager(lstobj, generate_objects=TRUE))
    expect_is(pobj, "ms_graph_pager")

    outobj <- pobj$value
    expect_is(outobj, "list")
    expect_true(all(sapply(outobj, inherits, "ms_object")))

    lstobj1 <- me$do_operation("memberOf", options=list(`$top`=1))
    pobj1 <- me$get_list_pager(lstobj1, generate_objects=TRUE)
    expect_is(pobj1, "ms_graph_pager")

    outobj1 <- list()
    for(i in seq_along(outobj))
        expect_equal(outobj[i], pobj1$value)
    expect_null(pobj1$value)
})
