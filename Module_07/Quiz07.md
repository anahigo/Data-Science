# Quiz

**Question 1** - Which of the following commands imports data from SAS?

a) `vendas <- read_sas('pesquisa.csv')`

b) `vendas <- read.sas('vendas.sas7bdat')`

c) `vendas <- read_sas(vendas.sas7bdat)`

d) `vendas <- read_sas('vendas.sas7bdat')`

**Answer: d)**

---

**Question 2** - Which of the following packages allows Web Scraping in R?

a) `rvest`

b) `httr`

c) `RCurl`

d) All of the above

**Answer:d)** 

---

**Question 3** - Which of the following code snippets is incorrect?

a)

```
ddply(data, (manufacturer),
      summarize,
      avgcty = mean(cty),
      sdcty = sd(cty),
      maxhwy = max(hwy))
```

b)

```
ddply(data, .(manufacturer),
      summarize,
      avgcty = mean(cty),
      maxhwy = max(hwy))
```

c)

```
ddply(data, .(cty),
      summarize,
      avgcty = mean(cty),
      sdcty = sd(cty),
      maxhwy = max(hwy))
```

d)

```
ddply(data, .(manufacturer),
      summarize,  avgcty = mean(cty),
      sdcty = sd(cty),
      maxhwy = max(hwy))
```

**Answer: a)**

---

**Question 4** - Which of the following code snippets is incorrect?

a)

```
iris_modif <- reshape(iris,
                      varying = 1:4,
                      v.names = 'Medidas',
                      timevar = "Dimensoes",
                      times = names(iris)[1:4],
                      idvar = "ID",
                      direction = "long")
```

b)

```
iris_modif <- reshape(iris,
                      varying = 1:4,
                      v.names = "Medidas",
                      timevar = "Dimensoes",
                      times = names(iris)[1:4],
                      idvar = "Id",
                      direction = "long")
```

c)

```
iris_modif <- reshape(iris,
                      varying = 1:6,
                      v.names = "Medidas",
                      timevar = "Dimensoes",
                      times = names(iris)[1:4],
                      idvar = "ID",
                      direction = "long")
```

d)

```
iris_modif <- reshape(iris,
                      varying = 1:4,
                      v.names = "Medidas",
                      timevar = "Dimensoes",
                      times = names(iris)[1:4],
                      idvar = "ID",
                      direction = "long")
```

**Answer: c)**

**Question 5** - The `melt()` function is part of the reshape2 package.

a) True

b) False

**Answer: a)**





