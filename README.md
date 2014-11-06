readSMI
=======

Reads SMI eye-tracking files.

###How to install:##

```R
install.packages("devtools")
library(devtools)
install_github("thohag/readSMI")
```

###How to use:###
Convert your iViewX data files (idf) to plain text with the IDF Converter before using this package.

```R
library(readSMI)
data = readSMI("plain_text_idf_export.txt")
```
