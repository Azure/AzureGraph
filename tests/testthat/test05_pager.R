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

    # assume result fits on 1 page
    expect_true(p$has_data())
    out <- p$value
    expect_is(out, "list")
    expect_null(p$value)
    expect_false(p$has_data())

    # return lists
    lst1 <- me$do_operation("memberOf", options=list(`$top`=1))
    p1 <- me$get_list_pager(lst1, generate_objects=FALSE)
    expect_is(p1, "ms_graph_pager")
    expect_true(p1$has_data())

    for(i in seq_along(out))
        expect_identical(out[i], p1$value)
    expect_null(p1$value)
    expect_false(p1$has_data())
})


test_that("Page result as data frame works",
{
    lstdf <- me$do_operation("memberOf", simplify=TRUE)
    expect_silent(pdf <- me$get_list_pager(lstdf))
    expect_is(pdf, "ms_graph_pager")

    expect_true(pdf$has_data())
    outdf <- pdf$value
    expect_is(outdf, "data.frame")
    expect_null(pdf$value)
    expect_false(pdf$has_data())

    # return data frames
    lstdf1 <- me$do_operation("memberOf", options=list(`$top`=1), simplify=TRUE)
    pdf1 <- me$get_list_pager(lstdf1)
    expect_is(pdf1, "ms_graph_pager")
    expect_true(pdf1$has_data())

    outdf1 <- list()
    for(i in seq_len(nrow(outdf)))
        outdf1 <- c(outdf1, list(pdf1$value))
    outdf1 <- do.call(vctrs::vec_rbind, outdf1)
    expect_identical(outdf, outdf1)
    expect_null(pdf1$value)
    expect_false(pdf1$has_data())
})


test_that("Page result as object works",
{
    lstobj <- me$do_operation("memberOf")
    expect_silent(pobj <- me$get_list_pager(lstobj, generate_objects=TRUE))
    expect_is(pobj, "ms_graph_pager")

    expect_true(pobj$has_data())
    outobj <- pobj$value
    expect_is(outobj, "list")
    expect_true(all(sapply(outobj, inherits, "ms_object")))
    expect_false(pobj$has_data())

    lstobj1 <- me$do_operation("memberOf", options=list(`$top`=1))
    pobj1 <- me$get_list_pager(lstobj1, generate_objects=TRUE)
    expect_is(pobj1, "ms_graph_pager")
    expect_true(pobj1$has_data())

    outobj1 <- list()
    for(i in seq_along(outobj))
        expect_equal(outobj[i], pobj1$value)
    expect_true(is_empty(pobj1$value))
    expect_false(pobj1$has_data())
})


test_that("extract_list_values works",
{
    # assume result fits on 1 page
    lst <- me$do_operation("memberOf")
    p <- me$get_list_pager(lst, generate_objects=FALSE)
    out <- p$value

    lst1 <- me$do_operation("memberOf", options=list(`$top`=1))
    p1 <- me$get_list_pager(lst1, generate_objects=FALSE)
    out1 <- extract_list_values(p1)

    expect_identical(out, out1)
    expect_error(extract_list_values(p1))
})


test_that("extract_list_values works for data frame",
{
    lst <- me$do_operation("memberOf", simplify=TRUE)
    p <- me$get_list_pager(lst, generate_objects=FALSE)
    out <- p$value

    lst1 <- me$do_operation("memberOf", options=list(`$top`=1), simplify=TRUE)
    p1 <- me$get_list_pager(lst1, generate_objects=FALSE)
    out1 <- extract_list_values(p1)

    expect_identical(out, out1)
    expect_error(extract_list_values(p1))
})


test_that("extract_list_values works for objects",
{
    lst <- me$do_operation("memberOf", simplify=FALSE)
    p <- me$get_list_pager(lst, generate_objects=TRUE)
    out <- p$value

    lst1 <- me$do_operation("memberOf", options=list(`$top`=1), simplify=FALSE)
    p1 <- me$get_list_pager(lst1, generate_objects=TRUE)
    out1 <- extract_list_values(p1)

    expect_equal(out, out1)
    expect_error(extract_list_values(p1))
})


test_that("Extra arguments work",
{
    testclass <- R6::R6Class("testclass",
    public=list(
        initialize=function(token, tenant, properties, arg1=NULL)
        {
            if(is.null(arg1)) stop("arg1 must not be NULL", call.=FALSE)
        }
    ))

    lst <- list(
        nextlink=NULL,
        valuelist=list(
            list(x=1),
            list(x=2),
            list(x=3)
        )
    )

    pager <- me$get_list_pager(lst, next_link_name="nextlink", value_name="valuelist", generate_objects=TRUE,
            default_generator=testclass, arg1=42)

    expect_is(pager, "ms_graph_pager")
    expect_true(pager$has_data())
    vals <- pager$value
    expect_is(vals, "list")
    expect_true(all(sapply(vals, inherits, "testclass")))

    register_graph_class("testclass", testclass, function(props) !is.null(props$x))
    pager2 <- me$get_list_pager(lst, next_link_name="nextlink", value_name="valuelist", generate_objects=TRUE,
            type_filter="testclass", arg1=42)

    expect_is(pager2, "ms_graph_pager")
    expect_true(pager2$has_data())
    vals2 <- pager2$value
    expect_is(vals2, "list")
    expect_true(all(sapply(vals2, inherits, "testclass")))
})
