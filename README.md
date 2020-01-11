readSMI
=======

Reads SMI eye-tracking files.

## How to install

```R
install.packages("devtools")
library(devtools)
install_github("thohag/readSMI")
```

## How to use

Convert your iViewX data files (idf) to plain text with SMI Vision's IDF Converter before using this package. [Reportedly](https://en.wikipedia.org/wiki/SensoMotoric_Instruments), SMI Vision was acquired by Apple in 2017 and the [IDF Converter is currently only _officially_ available as part of an iTools bundle](https://stackoverflow.com/q/50413910/1169233).

```R
library(readSMI)
data = readSMI("plain_text_idf_export.txt")
```
