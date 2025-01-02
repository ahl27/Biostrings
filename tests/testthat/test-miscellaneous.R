## Miscellaneous tests
## these are all relatively low priority and/or for files with only a few things
## some tests are just for internal functions

test_that("coloring works for DNA, RNA, and AA", {
    ## not a super important test
    make_DNA_AND_RNA_COLORED_LETTERS <-
        Biostrings:::make_DNA_AND_RNA_COLORED_LETTERS
    make_AA_COLORED_LETTERS <- Biostrings:::make_AA_COLORED_LETTERS

    dna_rna_expected <- c(DNA_BASES, "U", DNA_ALPHABET[-c(1:4,16:18)])
    expect_true(!any(duplicated(make_DNA_AND_RNA_COLORED_LETTERS())))
    expect_equal(sort(names(make_DNA_AND_RNA_COLORED_LETTERS())),
                 sort(dna_rna_expected))

    aa_expected <- AA_ALPHABET[-c(27:30)]
    expect_true(!any(duplicated(make_AA_COLORED_LETTERS())))
    expect_equal(sort(names(make_AA_COLORED_LETTERS())), sort(aa_expected))
})

test_that("users can update color palettes", {
    colored_letter <- \(letter, fg, bg){
        crayon::make_style(bg, bg=TRUE)(crayon::make_style(fg)(letter))
    }

    dnapalette <- get("DNA_AND_RNA_COLORED_LETTERS", envir=Biostrings:::.pkgenv)
    aapalette <- get("AA_COLORED_LETTERS", envir=Biostrings:::.pkgenv)
    bpalette <- get("BSTRING_COLORED_LETTERS", envir=Biostrings:::.pkgenv)

    origdna_palette <- Biostrings:::make_DNA_AND_RNA_COLORED_LETTERS()
    origaa_palette <- Biostrings:::make_AA_COLORED_LETTERS()
    origb_palette <- character(0L)

    ## check initialization
    expect_identical(dnapalette, origdna_palette)
    expect_identical(aapalette, origaa_palette)
    expect_identical(bpalette, origb_palette)

    ## check DNA update
    DNA_palette <- list(
      A=list(fg="blue",bg="black"),
      T=list(fg="red",bg='black'),
      G=list(fg='green',bg='black'),
      C=list(fg='yellow',bg='black')
    )
    update_DNA_palette(DNA_palette)

    dnapalette <- get("DNA_AND_RNA_COLORED_LETTERS", envir=Biostrings:::.pkgenv)
    expect_identical(dnapalette[c("A","T","G","C")],
                    c(A=colored_letter("A", "blue", "black"),
                      T=colored_letter("T", "red", "black"),
                      G=colored_letter("G", "green", "black"),
                      C=colored_letter("C", "yellow", "black")))
    update_DNA_palette()
    dnapalette <- get("DNA_AND_RNA_COLORED_LETTERS", envir=Biostrings:::.pkgenv)
    expect_identical(dnapalette, origdna_palette)

    ## Check AA update
    AA_palette <- list(
      A=list(fg="white", bg="purple"),
      B=list(fg=rgb(1,1,1), bg='orange')
    )
    update_AA_palette(AA_palette)
    aapalette <- get("AA_COLORED_LETTERS", envir=Biostrings:::.pkgenv)
    expect_identical(aapalette[c("A","B")],
                    c(A=colored_letter("A","white","purple"),
                      B=colored_letter("B", rgb(1,1,1), "orange")))
    update_AA_palette()
    aapalette <- get("AA_COLORED_LETTERS", envir=Biostrings:::.pkgenv)
    expect_identical(aapalette, origaa_palette)

    B_palette <- list(
      A=list(bg='green'),
      B=list(bg="red"),
      C=list(bg='blue'),
      D=list(fg="orange"),
      E=list(fg="yellow")
    )
    update_B_palette(B_palette)
    bpalette <- get("BSTRING_COLORED_LETTERS", envir=Biostrings:::.pkgenv)
    expect_identical(bpalette[c("A","B","C","D","E")],
                    c(A=colored_letter("A", rgb(1,1,1), "green"),
                      B=colored_letter("B", rgb(1,1,1), "red"),
                      C=colored_letter("C", rgb(1,1,1), "blue"),
                      D=crayon::make_style("orange")("D"),
                      E=crayon::make_style("yellow")("E")))
    update_B_palette()
    bpalette <- get("BSTRING_COLORED_LETTERS", envir=Biostrings:::.pkgenv)
    expect_identical(bpalette, origb_palette)
})

test_that("utils functions work as they should", {
    expect_true(Biostrings:::isNumericOrNAs(NA_character_))
    expect_true(Biostrings:::isNumericOrNAs(NA_real_))
    expect_true(Biostrings:::isNumericOrNAs(1))
    expect_true(Biostrings:::isNumericOrNAs(1L))
    expect_true(Biostrings:::isNumericOrNAs(1:2))
    expect_false(Biostrings:::isNumericOrNAs(NULL))
    expect_false(Biostrings:::isNumericOrNAs("a"))

    expect_error(Biostrings:::pow.int("a", 1), "must be a numeric vector")
    expect_identical(Biostrings:::pow.int(3,5), as.integer(3**5))

    expect_error(Biostrings:::normargUseNames(NA), "must be TRUE or FALSE")
    expect_true(Biostrings:::normargUseNames(NULL))
    expect_true(Biostrings:::normargUseNames(TRUE))
    expect_false(Biostrings:::normargUseNames(FALSE))
})

test_that("longestConsecutive still functions", {
    ## adapted from the examples in the man page
    v <- c("AAACTGTGFG", "GGGAATT", "CCAAAAAAAAAATT")
    expect_equal(longestConsecutive(v, "A"), c(3L, 2L, 10L))
    expect_equal(longestConsecutive(v, "C"), c(1L, 0L, 2L))
    expect_equal(longestConsecutive(v, "C"), c(1L, 0L, 2L))
    expect_error(longestConsecutive(v, NA),
                 "'letter' must be a character variable")
    expect_error(longestConsecutive(NA, "A"), "'x' must be a string")
})

test_that("matchprobes is deprecated", {
    expect_warning(matchprobes("A","A"),
                   "matchprobes() is deprecated.", fixed=TRUE)
})

