context("Batch request")

tenant <- Sys.getenv("AZ_TEST_TENANT_ID")
user <- Sys.getenv("AZ_TEST_USERPRINCIPALNAME")

if(tenant == "" || user == "")
    skip("Batch tests skipped: login credentials not set")

if(!interactive())
    skip("Batch tests skipped: must be in interactive session")

gr <- create_graph_login(tenant=tenant)


test_that("Simple batch request works",
{
    req1 <- graph_request$new("me")
    expect_is(req1, "graph_request")
    expect_identical(req1$op, "me")
    expect_identical(req1$method, "GET")

    bat1 <- req1$batchify()
    expect_identical(bat1, list(
        id=NULL,
        method="GET",
        url="/me"))

    req2 <- graph_request$new("me/ownedObjects")

    res <- gr$call_batch_endpoint(list(req1, req2))
    expect_is(res, "list")
    expect_identical(res[[1]]$id, "1")
    expect_identical(res[[2]]$id, "2")

    expect_identical(res[[1]]$body$`@odata.context`, "https://graph.microsoft.com/v1.0/$metadata#users/$entity")
    expect_identical(res[[2]]$body$`@odata.context`, "https://graph.microsoft.com/v1.0/$metadata#directoryObjects")
})


test_that("Batch request with dependency works",
{
    newname <- paste0(sample(letters, 20, TRUE), collapse="")
    req_get <- graph_request$new(file.path("users", user))
    req_update <- graph_request$new(file.path("users", user),
        body=list(givenName=newname), http_verb="PATCH")

    # deliberately out of order
    res <- gr$call_batch_endpoint(list(req_update, req_get, req_get), depends_on=c("1"=2, "3"=1))
    expect_is(res, "list")
    expect_identical(res[[1]]$id, "1")
    expect_identical(res[[2]]$id, "2")
    expect_identical(res[[3]]$id, "3")

    expect_false(identical(res[[1]]$body$givenName, newname))
    expect_identical(res[[3]]$body$givenName, newname)
})


test_that("Batch request errors handled gracefully",
{
    req1 <- graph_request$new("me")
    req2 <- graph_request$new("me/drive", body=list(foo="bar"), http_verb="POST")

    expect_error(gr$call_batch_endpoint(list(req1, req2)), "Graph batch job encountered errors")
})
