context("Test removal of non-ASCII characters.")

test_that("rm_accent is the converted version of a string with all non-ASCII characters removed.", {
  
  if(.Platform$OS.type == "unix"){
  #symbols
    acute = "áéíóúÁÉÍÓÚýÝ"
    #grave = "àèìòùÀÈÌÒÙ"
    #circunflex = "âêîôûÂÊÎÔÛ"
    #tilde = "ãõÃÕñÑ"
    #umlaut = "äëïöüÄËÏÖÜÿ"
    #cedil = "çÇ"
  } else {
    acute = "\u00e1\u00e9\u00ed\u00f3\u00fa\u00c1\u00c9\u00cd\u00d3\u00da\u00fd\u00dd"
    #grave = "àèìòùÀÈÌÒÙ"
    #circunflex = "âêîôûÂÊÎÔÛ"
    #tilde = "ãõÃÕñÑ"
    #umlaut = "äëïöüÄËÏÖÜÿ"
    #cedil = "çÇ"
  }
  
  #nudeSymbols
    nudeAcute = "aeiouAEIOUyY"
    nudeGrave = "aeiouAEIOU"
    nudeCircunflex = "aeiouAEIOU"
    nudeTilde = "aoAOnN"
    nudeUmlaut = "aeiouAEIOUy"
    nudeCedil = "cC"
  
  expect_equal(rm_accent(acute), nudeAcute)
  #expect_equal(rm_accent(grave), nudeGrave)
  #expect_equal(rm_accent(circunflex), nudeCircunflex)
  #expect_equal(rm_accent(tilde), nudeTilde)
  #expect_equal(rm_accent(umlaut), nudeUmlaut)
  #expect_equal(rm_accent(cedil), nudeCedil)
})