NEWS
================
R. Mark Sharp
03/30/2020

# snprcspf 1.1.6 (20200406)

  - Changed default blank replacement from “.” to " " in calls to
    read.xlsx(). The period were causing the column match routines to
    fail.

# snprcspf 1.1.5 (20200403)

  - Changed positive cell markup to yellow characters and red background
  - Changed to a conditional test for NA delimited columns of the
    divisor data. read\_excel, which had been used, always added NA
    values. read.xlsx does not.

# snprcspf 1.1.2 (20200330)

  - Used `suppressMessages` on `read_excel` to suppress the New Name
    messages

# snprcspf 1.1.1 (20200329)

  - Moved misplaced unit test file.

# snprcspf 1.1.0 (20200328)

  - Have replaced dependence on Java for the Excel formatting. Using
    openxlsx package
  - Have cleaned up code for file name and file path management for the
    Excel input file names and paths

# snprcspf 1.0.9 (20200321)

  - Copied build instructions from nprcgenekeepr
  - Removed redundant copy of a function.

# snprcspf 1.0.8 (20200321)

  - Separated R code into seperate files for each function.
  - Created some data objects for testing and demonstration. This has
    created a dependency on R \>= 3.5.0.

# snprcspf 1.0.7 (20190526)

  - Removed formating of Excel sheets to remove dependency on rJava used
    by XLConnect. As of 2019 May 26, Java 12 has a bug  that prevents
    the use of the XLConnect package. This version of snprcspf has
    removed that dependency with the side effect of not providing
    formating within the Excel workbook generated.
