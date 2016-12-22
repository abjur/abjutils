context("Remove non-ASCII characters.")

test_that("rm_accent is the converted version of a string with all non-ASCII characters removed.", {
  
  if(.Platform$OS.type == "unix"){
  #symbols
    acute = "áéíóúÁÉÍÓÚýÝ"
    grave = "àèìòùÀÈÌÒÙ"
    circunflex = "âêîôûÂÊÎÔÛ"
    tilde = "ãõÃÕñÑ"
    umlaut = "äëïöüÄËÏÖÜÿ"
    cedil = "çÇ"
  } else {
    acute = iconv("\u00e1\u00e9\u00ed\u00f3\u00fa\u00c1\u00c9\u00cd\u00d3\u00da\u00fd\u00dd", from = "utf-8", "latin1")
    grave = iconv("\u00e0\u00e8\u00ec\u00f2\u00f9\u00c0\u00c8\u00cc\u00d2\u00d9", from = "utf-8", "latin1")
    circunflex = iconv("\u00e2\u00ea\u00ee\u00f4\u00fb\u00c2\u00ca\u00ce\u00d4\u00db", from = "utf-8", "latin1")
    tilde = iconv("\u00e3\u00f5\u00c3\u00d5\u00f1\u00d1", from = "utf-8", "latin1")
    umlaut = iconv("\u00e4\u00eb\u00ef\u00f6\u00fc\u00c4\u00cb\u00cf\u00d6\u00dc\u00ff", from = "utf-8", "latin1")
    cedil = iconv("\u00e7\u00c7", from = "utf-8", "latin1")
  }
  
  #nudeSymbols
    nudeAcute = "aeiouAEIOUyY"
    nudeGrave = "aeiouAEIOU"
    nudeCircunflex = "aeiouAEIOU"
    nudeTilde = "aoAOnN"
    nudeUmlaut = "aeiouAEIOUy"
    nudeCedil = "cC"
  
  expect_equal(rm_accent(acute), nudeAcute)
  expect_equal(rm_accent(grave), nudeGrave)
  expect_equal(rm_accent(circunflex), nudeCircunflex)
  expect_equal(rm_accent(tilde), nudeTilde)
  expect_equal(rm_accent(umlaut), nudeUmlaut)
  expect_equal(rm_accent(cedil), nudeCedil)
})