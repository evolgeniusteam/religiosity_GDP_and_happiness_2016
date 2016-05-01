library("xlsx");

## -- r == RELIGIOSITY, 即：被调查人自认为是有信仰的百分比；数据来源：http://www.wingia.com/web/files/news/14/file/14.pdf
r <- read.xlsx(file ="data.xlsx", 1, row.names = 1, header = F);

## -- h == happiness, https://en.wikipedia.org/wiki/Satisfaction_with_Life_Index
h <- read.xlsx(file ="data.xlsx", 2, row.names = 1, header = F);

## -- p == 人均GDP: https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)_per_capita
p <- read.xlsx(file ="data.xlsx", 3, row.names = 1, header = F);


## ========== 
## test1: 钱和信仰的关系
rp <- data.frame( rel = r$X2, ppp = p[ rownames(r), "X2" ], row.names = rownames(r) );
rp <- rp[ complete.cases(rp), ];
dim(rp);
cor.test(rp$rel, rp$ppp);

## ========== 
## test2: 钱和幸福的关系
hp <- data.frame( happiness = h$X2, ppp = p[ rownames(h), "X2" ], row.names = rownames(h) );
hp <- hp[ complete.cases(hp), ];
dim(hp);
cor.test(hp$happiness, hp$ppp);

## ========== 
## test3: 信仰和幸福的关系
rh <- data.frame( happiness = h$X2, rel = r[ rownames(h), "X2" ], row.names = rownames(h) );
rh <- rh[ complete.cases(rh), ];
dim(rh);
cor.test(rh$happiness, rh$rel);

## ========== 
## test4: 信仰和幸福的关系，扣除钱的影响 --
rhp <- data.frame( happiness = h$X2, rel = r[ rownames(h), "X2" ],  ppp = p[ rownames(h), "X2" ],  row.names = rownames(h) );
rhp <- rhp[ complete.cases(rhp), ];
dim(rhp);
cor.test(rhp$happiness, rhp$rel);
library("ppcor");
pcor.test( rhp$happiness, rhp$rel, rhp$ppp );

## ========== 
## test5: 幸福和钱的关系，扣除信仰和影响 --
pcor.test( rhp$happiness, rhp$ppp, rhp$rel );
