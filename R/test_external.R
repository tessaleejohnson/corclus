test_external <-
  function(
    args
  ) {

    .p <- progressr::progressor(steps = max(lengths(args)))

    furrr::future_pmap(
      .l = args,
      .f = test_internal,
      progress_bar = .p
    )


  }
