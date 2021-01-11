context("Class registration")

test_that("Class registration works",
{
    newclass <- R6::R6Class("newclass", inherit=ms_object)
    expect_false("newclass" %in% ls(.graph_classes))
    expect_silent(register_graph_class("newclass", newclass, function(x) FALSE))
    expect_true("newclass" %in% ls(.graph_classes))
    expect_error(register_graph_class("badclass", "badclassname", FALSE))
})
