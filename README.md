# Protocol

Comparison of RPGeNet v1 and v2 interactomes using hiveplots.


> Skeleton.old has 1287 nodes. Skeleton has 3847


```{r}
source("hiveplot.R");
drivers <- read.csv(file="rpgenet_nodes_string_filtered_2018.csv")
drivers <- drivers[drivers$gene_disease > 0,]
rownames(drivers) <- drivers$identifier;
skeleton <- read.csv(file="skeleton.csv", header=F);
graph2Hive(skeleton, "v2_hiveplot", clusterSize=125, drivers);


drivers.old <- read.csv(file="drivers.old.csv");
rownames(drivers.old) <- drivers.old$identifier;
skeleton.old <- read.csv(file="RPgenes-x-alldbs.GO_graph_skeleton.csv", header=F);
skeleton.old$V3 <- 1
graph2Hive(skeleton.old, "v1_hiveplot", clusterSize=150, drivers.old);
```


## Version 1 top 5 drivers 


|    2 |    3  |
| ---  | ----- |
| 1107 |  180 |


|   name      | degree  |   betw  |  size  | color   | axis |
| ----------- | ------ | ------ | ----- | ------- | ----- |
|   <chr>     |  <dbl> |   <dbl>  | <dbl>  | <fct>   |<int> |
| 1 SNRNP200  |    86. |  0.0745  |  1.98  | #FE0D0B |    2 |
| 2 CRX       |    77. |  0.0804  |  1.99  | #FE1410 |    2 |
| 3 PRPF6     |    85. |  0.0705  |  1.98  | #FE110D |    2 |
| 4 DHX38     |    72. |  0.0507  |  1.97  | #FD1B16 |    2 |
| 5 PRPF4     |    75. |  0.0672  |  1.98  | #FD1713 |    2 |
| 6 PRPF31    |    85. |  0.110   |  1.99  | #FE110D |    3 |
| 7 RHO       |    60. |  0.0703  |  1.98  | #FC2F26 |    3 |
| 8 NEUROD1   |    49. |  0.0433  |  1.96  | #FC4034 |    3 |
| 9 IQCB1     |    43. |  0.0530  |  1.98  | #FB5142 |    3 |
|10 HK1       |    44. |  0.0518  |  1.97  | #FB4E3F |    3 |



## Version 2 top 5 drivers 

|   2 |    3 |     4 |     5 | 
| ---- | ---- | ----- | ----- |
| 2982 |  415 |   239 |   211 |

|    name  | degree  |  betw  | size | color  |   axis |
| ----------- | ------ | ------ | ----- | ------- | ----- |
|    <chr>|   <dbl> |  <dbl>| <dbl> | <fct>|   <int> |
|  1 RB1|      174. | 0.230|   2.00 | #FE110E |    2|
|  2 IQCB1|    179. | 0.129|   1.99 | #FE0E0B |    2|
|  3 TCTN2|    173. | 0.147|   1.99 | #FE130F |    2|
|  4 OFD1|     185. | 0.116|   1.98 | #FE0C0A |    2|
|  5 SCLT1|    173. | 0.104|   1.98 | #FE130F |    2|
|  6 GNAT2|     84. | 0.0353|  1.92 | #FA5C4A |    3|
|  7 TIMP3|     95. | 0.0838|  1.97 | #FB4F40 |    3|
|  8 GNB3|     134. | 0.107|   1.98 | #FC3027 |    3|
|  9 RHO|       88. | 0.0579|  1.95 | #FB5746 |    3|
| 10 GRM6|      90. | 0.0450|  1.94 | #FB5443 |    3|
| 11 LRP5|      66. | 0.0499|  1.95 | #F9765F |    4|
| 12 TRIM32|   158. | 0.184|   1.99 | #FD1B16 |    4|
| 13 POC1B|     31. | 0.0206|  1.86 | #F7AD8B |    4|
| 14 CTNNA1|    79. | 0.0636|  1.96 | #FA624F |    4|
| 15 SAG|       25. | 0.0106|  1.76 | #F6B793 |    4|
| 16 ACO2|      35. | 0.0323|  1.91 | #F7A686 |    5|
| 17 PEX2|      54. | 0.0423|  1.93 | #F8886D |    5|
| 18 HK1|       36. | 0.0344|  1.91 | #F7A585 |    5|
| 19 NMNAT1|    32. | 0.0200|  1.85 | #F7AB8A |    5|
| 20 OTX2|      37. | 0.0340|  1.91 | #F7A383 |    5|
