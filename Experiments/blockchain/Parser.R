### This script converts the block chain transaction data into a network ###

# Read the JSON file containing tokens
library(jsonlite)
library(lubridate)

root <- file <- "C:/Users/owais/Dropbox/Knowledge/PhD/Datasets/Blockchain/"
token_file <- paste(root, "erc20tokens.json", sep='')
blocktime_file <- paste(root, "eth-blocktime-0-999999.txt", sep='')
transaction_file <- paste(root, "eth-tx-0-999999.txt", sep='')

# Read tokens to DataFrame
tokens <- fromJSON(token_file, simplifyDataFrame=TRUE)
head(tokens)

# Read block times to DataFrame
blocktimes <- read.csv2(blocktime_file, header=FALSE, sep=' ', col.names=c("block", "unix_timestamp"))
tail(blocktimes)

# Read transaction data to DataFrame
#<SYMBOL> <blockno> <tx no> <from addr> <to addr> <value>
transactions <- read.csv2(transaction_file, header=FALSE, sep=' ', col.names=c("symbol", "block", "tx_no", "from", "to", "value"),
                          colClasses=c('character', 'numeric', 'numeric', 'character', 'character', 'numeric'),
                          stringsAsFactors=FALSE, nrows=100000)
head(transactions)

# Remove all Failed transactions and Drop the fields other than from and to
transactions <- transactions[transactions$symbol == "ETH",]
transactions[, c("symbol", "block", "tx_no", "value")] <- NULL
head(transactions)

write.table(transactions, paste(root, "edgelist.txt"), row.names=FALSE, col.names=FALSE, sep=' ', quote=FALSE)

