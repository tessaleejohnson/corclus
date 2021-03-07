test_internal <-
  function(
    a,
    b,
    c#,
    #progress_bar
  ) {

    #progress_bar()

    Sys.sleep(5)

    list((a + b + c))

  }
