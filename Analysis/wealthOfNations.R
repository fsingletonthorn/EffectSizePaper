library(readxl)
library(Hmisc)
library(stringr)
library(SASxport)
library(tidyverse)
options(stringsAsFactors = FALSE)

data <- read_excel("Data/wealth-per-adult-2016-2017.xlsx")
difference <- data$`Average Wealth per Adult (USD)`[data$X__1=="United States"] - data$`Average Wealth per Adult (USD)`[data$X__1=="Russia"]
sdAll <- sd(data$`Average Wealth per Adult (USD)`)
difference/sdAll

### Figuring out height sds from The Evolution of Adult Height in Europe: A Brief Note (2007) Jaume Garcia & Climent Quintana-Domeque 
# Table 1A. Number of observations by year of birth, men
years <- "1950 1951 1952 1953 1954 1955 1956 1957 1958 1959 1960 1961 1962 1963 1964 1965"
ns <-"Austria 119 199 173 168 187 177 180 228 231 190 175 197 222 220 166 193
Belgium 131 140 146 122 163 144 158 180 179 258 260 217 164 219 175 172
Denmark 124 177 104 141 97 150 148 145 185 146 147 123 195 148 152 170
Finland 189 146 173 157 182 133 194 147 141 167 166 154 166 156 163 145
Greece 222 313 243 260 286 284 267 277 304 326 257 299 230 297 262 272
Ireland 176 102 171 143 170 151 177 160 177 158 134 145 153 159 127 174
Italy 435 431 434 426 464 510 409 394 453 532 527 485 523 473 604 597
Portugal 259 315 298 294 311 307 330 298 325 265 299 307 326 365 295 305
Spain 263 345 293 315 345 334 346 362 354 364 412 421 388 352 405 400
Sweden 136 140 147 135 125 150 135 145 153 115 147 155 134 153 174 169"
splitNs <- str_split(ns, pattern = c("\n"))
splitNsList <- mapply(splitNs, FUN = str_split, pattern = c(" "))
splitNsCountsMale <- data.frame(splitNsList[1:10])[2:16, 1:10]

ns2 <- "Austria 178 228 203 214 168 208 195 190 202 191 236 211 156 105 53
Belgium 125 92 139 142 142 147 169 93 112 86 105 132 71 67 B
Denmark 183 99 150 137 120 164 179 119 106 213 107 84 81 54 A
Finland 138 143 137 141 108 102 154 119 136 130 164 189 180 104 69
Greece 303 293 234 360 304 313 305 247 252 308 296 261 253 157 61
Ireland 107 157 141 106 179 121 190 163 159 205 191 199 161 107 54
Italy 664 620 625 559 636 593 684 616 621 515 591 489 395 237 106
Portugal 304 256 308 284 391 426 509 393 436 543 476 398 288 204 87
Spain 474 486 459 466 511 484 496 396 535 472 525 571 332 203 111
Sweden 157 171 146 136 135 148 154 158 140 140 149 127 85 B B"
splitNs2 <- str_split(ns2, pattern = c("\n"))
splitNsList2 <- mapply(splitNs2, FUN = str_split, pattern = c(" "))
splitNsCountsMale2 <- data.frame(splitNsList2[1:10])[2:16, 1:10]

NsCountsMale <- cbind(splitNsCountsMale,splitNsCountsMale2)

# Table 5A. Standard deviation of heights by year of birth, men, centimeters 
sds <- "Austria 7.0 7.8 7.9 6.2 7.6 6.4 6.4 5.4 7.5 6.5 7.9 5.5 7.7 6.5 6.5 6.0
Belgium 7.4 5.5 7.0 7.8 6.5 6.2 5.9 7.2 6.9 6.6 6.6 7.1 8.2 7.9 5.0 7.3
Denmark 5.8 7.7 7.1 7.6 6.9 6.0 6.1 6.6 5.4 5.4 7.0 5.0 5.6 5.8 5.8 9.3
Finland 6.4 5.3 5.6 5.7 6.2 7.6 6.8 5.9 6.7 6.7 7.0 5.9 7.9 7.1 6.7 5.7
Greece 6.2 6.1 7.4 6.7 7.1 6.8 6.3 6.7 6.3 6.6 6.7 5.8 6.7 5.4 6.3 6.6
Ireland 7.0 7.2 8.3 4.8 6.7 6.9 6.1 5.5 7.9 6.8 6.5 6.2 7.2 5.5 7.1 6.7
Italy 5.8 6.8 7.0 8.1 6.7 6.2 7.2 7.0 6.6 6.0 7.4 7.0 6.7 8.0 7.1 6.5
Portugal 6.9 7.5 6.4 6.6 5.9 5.9 7.1 6.6 5.6 6.1 6.3 6.3 7.3 7.1 5.5 5.8
Spain 6.3 6.6 7.5 7.1 7.0 6.3 7.6 6.9 6.5 7.4 6.5 6.8 6.4 7.5 7.3 6.5
Sweden 6.9 7.0 7.3 7.3 6.6 6.8 6.3 7.2 7.0 7.1 6.6 6.9 7.4 6.6 7.7 7.5"
splitSD <- str_split(sds, pattern = c("\n"))
splitSDList <- mapply(splitSD, FUN = str_split, pattern = c(" "))
splitSDCountsMale <- data.frame(splitSDList[1:10])[2:16, 1:10]

sds2 <- "Austria 6.7 5.7 7.4 6.3 9.1 5.6 6.8 7.3 6.5 8.7 6.0 7.1 8.0 7.1 6.1
Belgium 7.9 5.0 5.8 6.6 6.6 9.0 7.3 7.0 8.7 6.1 10.1 6.0 6.0 8.6 6.6
Denmark 8.5 6.5 7.7 6.7 5.9 6.8 5.7 7.1 7.9 5.9 7.8 5.5 6.8 6.8 NA
Finland 7.5 5.6 6.2 7.1 6.5 6.8 5.9 6.9 6.5 7.9 6.2 7.2 7.4 8.4 5.9
Greece 6.7 6.1 8.6 6.5 6.5 7.6 6.3 6.8 7.8 7.0 6.2 6.4 7.6 7.6 7.6
Ireland 6.6 6.8 6.5 6.0 6.5 6.2 8.3 8.7 5.9 9.1 7.6 7.5 7.4 8.9 6.3
Italy 6.6 7.0 7.1 6.9 6.8 7.6 7.0 6.7 6.6 6.6 6.4 7.4 6.3 7.2 6.0
Portugal 6.5 6.2 6.2 6.3 5.7 6.4 6.1 6.7 6.8 7.6 7.5 6.8 7.7 7.6 8.2
Spain 7.6 6.6 7.8 7.1 6.5 6.4 7.4 7.5 7.3 8.6 6.7 7.3 8.1 7.4 6.9
Sweden 6.9 7.7 7.5 7.2 7.0 7.5 6.8 6.7 6.1 6.6 7.6 6.5 6.8 7.5 B"
splitSD2 <- str_split(sds2, pattern = c("\n"))
splitSDList2 <- mapply(splitSD2, FUN = str_split, pattern = c(" "))
splitSDCountsMale2 <- data.frame(splitSDList2[1:10])[2:16, 1:10]

SDCountsMale <- cbind( splitSDCountsMale,splitSDCountsMale2)


## Table 3a - average hieghts of men CM
heights <- "Austria 177.6 175.0 177.2 176.6 174.5 177.1 176.7 176.6 177.5 176.5 177.7 179.2 177.4 180.4 179.1 179.7
Belgium 172.9 176.7 176.6 176.3 178.1 176.5 177.7 178.4 177.2 176.3 176.8 176.8 175.7 178.2 177.4 178.0
Denmark 178.5 182.8 179.8 180.6 181.6 178.1 179.5 180.8 180.2 179.0 179.3 180.3 182.0 180.4 181.0 181.1
Finland 177.2 177.9 177.4 177.6 178.9 177.7 178.0 178.7 179.1 178.6 180.6 178.1 180.7 181.6 179.7 177.7
Greece 174.4 173.7 174.9 174.1 176.2 175.0 174.5 176.2 174.9 174.9 176.7 175.5 177.3 176.6 176.9 176.5
Ireland 173.8 174.8 175.4 176.1 174.8 174.8 176.3 175.9 174.7 175.0 179.4 176.6 175.9 175.1 176.2 176.7
Italy 171.3 171.6 172.7 173.7 171.8 173.9 174.4 174.0 174.4 174.6 174.0 173.0 175.3 175.3 176.0 174.9
Portugal 168.7 169.1 168.0 168.4 169.3 169.3 171.0 168.0 171.3 170.5 169.3 170.9 169.9 170.9 167.2 171.0
Spain 172.4 171.6 171.3 169.5 170.6 172.2 172.1 172.1 172.0 170.7 171.7 172.1 173.7 172.8 173.6 174.5
Sweden 179.0 179.8 179.7 179.6 180.4 179.5 179.1 179.6 180.2 178.9 179.1 180.4 180.9 180.7 181.3 181.2"
splitHeight <- str_split(heights, pattern = c("\n"))
splitHeightList <- mapply(splitHeight, FUN = str_split, pattern = c(" "))
splitHeightCountsMale <- data.frame(splitHeightList[1:10])[2:16, 1:10]

heights <- "Austria 178.8 179.5 180.1 179.5 174.7 179.7 178.1 180.5 178.5 176.9 179.6 180.3 178.2 180.5 179.2
Belgium 179.9 179.9 182.1 176.9 178.0 178.5 178.6 181.6 176.4 181.0 180.3 180.7 179.3 178.4 178.6
Denmark 182.0 180.9 182.4 182.3 180.8 178.7 180.2 182.1 182.1 183.6 181.7 186.0 182.6 185.4 NA
Finland 179.5 177.6 177.5 177.7 177.5 178.7 178.2 180.7 176.5 176.1 179.8 179.9 178.2 180.0 175.9
Greece 176.2 176.9 176.0 178.5 177.5 179.5 176.8 177.7 180.0 178.0 177.8 178.4 179.5 179.3 178.3
Ireland 178.1 177.4 175.3 176.0 177.7 179.2 175.9 175.5 178.1 176.2 176.6 179.3 175.4 178.3 177.5
Italy 173.9 175.6 175.0 174.3 174.7 175.6 174.0 174.6 176.8 175.7 176.1 175.9 177.8 178.4 177.2
Portugal 168.5 169.2 171.1 170.1 170.2 169.9 172.2 172.4 173.2 172.9 171.2 173.8 172.0 173.9 173.7
Spain 174.4 174.1 174.0 175.0 175.8 174.4 176.7 176.2 175.2 175.9 176.4 176.4 172.7 176.8 178.0
Sweden 180.0 181.0 181.3 180.2 179.9 180.6 180.8 180.2 180.6 179.9 181.4 181.0 180.9 181.3 B"
splitHeight2 <- str_split(heights, pattern = c("\n"))
splitHeightList2 <- mapply(splitHeight, FUN = str_split, pattern = c(" "))
splitHeightCountsMale2 <- data.frame(splitHeightList[1:10])[2:16, 1:10]

heightsCountsMale <- cbind(splitHeightCountsMale, splitHeightCountsMale2)
names(heightsCountsMale) <- 1:20

# Converting to numeric vectors
SDsMale <- as.vector(unlist(mutate_all(SDCountsMale, function(x) as.numeric(as.character(x)))))
nsMale <- as.vector(unlist(mutate_all(NsCountsMale, function(x) as.numeric(as.character(x)))))
heightsMale <- as.vector(unlist(mutate_all(heightsCountsMale, function(x) as.numeric(as.character(x)))))
# calculating mean and sd
maleSD <- sum(SDsMale* nsMale, na.rm=T)/sum(nsMale , na.rm=T)
maleHeightMean <- sum(heightsMale* nsMale, na.rm=T)/sum(nsMale, na.rm=T)



######### Woman #######
# Table 2A. Number of observations by year of birth, men
years <- "1950 1951 1952 1953 1954 1955 1956 1957 1958 1959 1960 1961 1962 1963 1964 1965"
ns <-"Austria 129 169 176 124 185 179 242 192 192 238 230 199 230 285 213 194
Belgium 159 139 168 127 144 175 216 172 236 200 248 227 221 221 216 212
Denmark 146 127 166 120 138 113 126 121 145 168 154 167 181 161 161 164
Finland 180 146 172 171 171 182 194 201 142 166 118 170 139 161 165 134
Greece 333 288 238 188 331 289 241 327 310 297 303 293 321 267 329 253
Ireland 143 135 147 156 150 141 138 192 205 163 120 163 138 146 170 138
Italy 426 424 361 443 514 468 517 523 471 507 523 526 477 521 551 535
Portugal 288 327 346 283 326 308 306 359 328 318 350 330 332 333 352 316
Spain 370 237 379 325 355 360 379 362 390 314 359 332 422 415 399 345
Sweden 170 144 146 175 148 152 142 142 143 157 151 143 142 166 173 165"
splitNs <- str_split(ns, pattern = c("\n"))
splitNsList <- mapply(splitNs, FUN = str_split, pattern = c(" "))
splitNsCountsFemale <- data.frame(splitNsList[1:10])[2:16, 1:10]

ns2 <- "Austria 188 170 131 177 206 149 189 191 177 171 206 155 132 77 69
Belgium 194 187 151 164 111 168 142 175 159 158 105 146 104 66 B
Denmark 186 143 152 115 147 157 173 135 143 163 90 117 80 52 B
Finland 159 170 128 146 126 136 91 141 121 119 156 180 237 116 90
Greece 234 267 306 281 268 262 344 298 335 264 286 322 316 184 77
Ireland 130 160 84 156 122 142 178 174 194 169 200 180 111 97 B
Italy 595 531 522 621 658 592 674 640 626 548 621 575 332 210 92
Portugal 322 246 297 233 353 306 371 364 363 451 516 438 320 214 106
Spain 421 453 479 469 510 494 481 479 531 519 469 501 318 236 94
Sweden 151 174 144 166 130 143 153 138 165 118 125 113 86 B B"
splitNs2 <- str_split(ns2, pattern = c("\n"))
splitNsList2 <- mapply(splitNs2, FUN = str_split, pattern = c(" "))
splitNsCountsFemale2 <- data.frame(splitNsList2[1:10])[2:16, 1:10]
NsCountsFemale <- cbind(splitNsCountsFemale,splitNsCountsFemale2)

# Table 6A. Standard deviation of heights by year of birth, women, centimeters 
sds <- "Austria 5.0 4.9 7.1 5.1 4.3 5.2 5.6 6.7 4.7 6.4 5.1 5.2 6.2 5.5 4.1 4.3
Belgium 5.7 8.2 6.6 5.8 5.9 5.8 6.0 5.5 6.0 5.9 6.0 6.6 6.6 6.5 5.8 5.9
Denmark 5.7 5.3 5.5 5.0 6.0 5.8 6.1 5.4 5.1 6.0 5.8 5.9 6.3 4.4 5.6 6.0
Finland 6.4 6.7 5.4 6.3 6.5 6.5 5.0 5.7 6.5 6.9 6.6 6.3 5.3 5.4 5.9 7.2
Greece 5.6 5.2 5.0 5.2 6.1 5.3 5.0 5.8 5.6 5.4 5.7 6.2 5.0 5.3 5.1 4.9
Ireland 6.7 6.6 6.3 6.6 5.7 6.7 6.0 6.1 7.3 6.6 5.2 7.4 7.0 5.4 7.2 7.9
Italy 6.6 6.9 5.6 5.8 6.6 6.8 5.8 5.6 6.0 5.7 5.8 6.1 5.5 5.9 6.3 5.6
Portugal 5.9 8.7 5.1 5.6 5.7 5.6 5.3 7.7 5.3 5.1 5.9 5.0 5.7 6.0 5.6 5.3
Spain 5.6 6.2 5.7 5.8 6.0 5.5 5.7 6.4 5.7 5.8 5.8 6.4 7.0 5.2 6.4 6.1
Sweden 5.6 6.7 5.8 5.3 6.2 6.0 6.8 6.8 5.2 6.5 6.7 6.5 6.4 6.2 5.7 5.7"
splitSD <- str_split(sds, pattern = c("\n"))
splitSDList <- mapply(splitSD, FUN = str_split, pattern = c(" "))
splitSDCountsFemale <- data.frame(splitSDList[1:10])[2:16, 1:10]

sds2 <- "Austria 5.2 5.7 5.6 6.3 12.3 5.5 5.8 5.4 5.0 6.3 4.9 5.8 4.9 4.6 5.6
Belgium 7.2 6.4 5.8 5.2 6.9 5.2 5.7 9.3 6.3 5.5 6.8 6.3 6.7 5.7 5.3
Denmark 6.8 6.6 7.0 7.1 6.8 5.5 5.5 5.0 5.5 5.5 4.3 5.6 7.4 4.3 7.4
Finland 5.8 6.7 6.2 7.6 6.2 5.9 4.1 11.7 6.1 4.3 6.0 6.3 5.3 6.7 6.5
Greece 5.4 4.9 6.2 5.6 5.1 6.2 5.4 6.1 6.6 5.3 5.1 5.9 4.6 6.7 7.3
Ireland 6.0 5.2 6.3 6.2 6.2 6.1 6.9 5.9 6.7 6.8 6.1 6.2 7.5 6.0 7.6
Italy 6.5 6.7 5.1 5.8 6.2 6.0 6.0 5.5 5.4 6.1 6.0 6.4 6.3 6.1 6.1
Portugal 5.8 4.3 5.4 6.2 6.1 4.7 6.0 6.7 6.9 7.0 6.4 5.9 5.9 5.8 5.3
Spain 5.5 6.2 5.4 7.0 5.3 6.1 6.2 6.5 5.4 6.9 5.4 7.2 5.9 6.6 6.2
Sweden 8.2 6.2 5.5 6.1 6.6 6.5 5.6 6.4 5.5 5.8 6.2 5.1 4.4 6.5 B"
splitSD2 <- str_split(sds2, pattern = c("\n"))
splitSDList2 <- mapply(splitSD2, FUN = str_split, pattern = c(" "))
splitSDCountsFemale2 <- data.frame(splitSDList2[1:10])[2:16, 1:10]

SDCountsFemale <- cbind( splitSDCountsFemale,splitSDCountsFemale2)

## Table 4a - average hieghts of woman CM
heights <- "Austria 164.9 164.1 165.4 165.6 166.6 166.8 166.4 166.6 165.8 165.7 166.5 165.9 165.8 166.6 167.1 168.1
Belgium 161.5 163.0 163.1 164.1 163.7 164.8 165.2 163.9 165.1 165.0 165.4 165.4 163.7 165.4 164.3 166.0
Denmark 167.4 166.7 168.5 167.8 168.0 164.7 165.8 167.0 166.3 168.1 165.9 167.0 167.4 167.6 166.1 169.4
Finland 164.1 165.1 163.4 164.4 164.5 164.5 164.2 164.1 165.8 165.1 165.1 166.3 165.0 165.6 165.0 165.7
Greece 163.3 163.0 162.7 163.6 163.8 163.6 163.8 164.4 163.9 163.9 164.4 164.3 164.6 164.3 164.9 166.1
Ireland 163.1 162.2 163.1 162.3 162.9 162.4 162.5 161.4 162.5 162.3 164.7 162.0 163.7 163.6 162.5 164.2
Italy 161.4 160.6 161.4 161.6 161.7 162.0 162.4 162.4 161.6 162.3 162.9 162.4 163.8 162.3 163.4 163.2
Portugal 157.7 158.7 160.3 159.3 159.3 158.2 159.1 157.9 160.4 160.1 159.0 162.2 160.5 159.0 160.1 160.5
Spain 159.9 160.6 160.3 159.5 161.1 160.8 159.3 160.4 160.7 162.3 162.1 160.8 161.1 160.9 161.9 162.0
Sweden 165.4 165.5 164.7 165.0 165.8 165.7 166.1 166.3 167.1 166.3 166.1 166.4 167.6 166.8 166.2 166.3"
splitHeight <- str_split(heights, pattern = c("\n"))
splitHeightList <- mapply(splitHeight, FUN = str_split, pattern = c(" "))
splitHeightCountsFemale <- data.frame(splitHeightList[1:10])[2:16, 1:10]

heights <- "Austria 166.1 167.3 164.9 166.9 166.2 166.8 168.6 167.2 167.7 168.1 166.5 166.5 168.7 166.2 167.6
Belgium 167.0 165.5 167.4 165.3 166.7 166.2 167.7 164.4 167.0 165.7 166.1 166.8 167.7 170.5 168.1
Denmark 167.8 168.6 168.1 168.9 169.0 166.4 167.5 167.9 168.7 169.9 170.7 167.0 168.7 167.5 169.1
Finland 167.0 166.4 165.3 163.9 164.9 166.2 164.3 163.5 166.1 165.9 166.5 164.6 165.9 167.6 165.0
Greece 165.3 165.8 165.4 165.7 165.3 166.8 166.5 166.7 166.3 165.9 166.0 165.6 165.9 165.5 166.6
Ireland 165.9 165.1 166.0 162.6 160.8 164.3 164.5 165.8 163.4 165.5 164.6 164.2 165.0 164.7 163.5
Italy 164.0 163.2 163.8 164.4 164.1 163.7 163.9 163.7 164.9 165.7 165.9 165.5 166.8 166.6 167.8
Portugal 160.0 161.5 161.4 162.0 159.1 162.2 160.8 162.4 163.4 163.6 161.8 162.6 162.2 162.3 163.7
Spain 161.9 162.4 162.4 163.6 163.7 163.9 164.2 164.7 164.7 164.7 164.0 165.0 165.3 167.0 166.2
Sweden 165.8 166.8 167.3 167.5 167.2 167.0 166.2 166.7 167.0 166.4 167.0 166.1 165.7 170.2 B"
splitHeight2 <- str_split(heights, pattern = c("\n"))
splitHeightList2 <- mapply(splitHeight, FUN = str_split, pattern = c(" "))
splitHeightCountsFemale2 <- data.frame(splitHeightList[1:10])[2:16, 1:10]

heightsCountsFemale <- cbind(splitHeightCountsFemale, splitHeightCountsFemale2)
names(heightsCountsFemale) <- 1:20

# Converting to numeric vectors
SDsFemale <- as.vector(unlist(mutate_all(SDCountsFemale, function(x) as.numeric(as.character(x)))))
nsFemale <- as.vector(unlist(mutate_all(NsCountsFemale, function(x) as.numeric(as.character(x)))))
heightsFemale <- as.vector(unlist(mutate_all(heightsCountsFemale, function(x) as.numeric(as.character(x)))))

femaleSD <- sum(SDsFemale* nsFemale, na.rm=T)/sum(nsFemale, na.rm=T)
femaleHeightMean <- sum(heightsFemale* nsFemale, na.rm=T)/sum(nsFemale, na.rm=T)

##### Calculating cohen's d 
sp <- sqrt( ((sum(nsFemale, na.rm = T)-1)*femaleSD^2 + (sum(nsMale, na.rm = T)-1)*maleSD^2) / (sum(nsMale, na.rm = T) + (sum(nsFemale, na.rm = T) -2)) )
d <- (femaleHeightMean - maleHeightMean) / sp






