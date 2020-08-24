use hclust\_examples to do hierarchical clustering
==================================================

set.seed(123)

library(ggplot2) library(LICORS) \# for kmeans++ library(foreach)
library(mosaic) library(cluster)

mkt\_seg = read.csv(‘social\_marketing.csv’, header=TRUE)

\#scales the data and takes out the first column with the id number,
spam, chatter, uncategorized, and adult S = mkt\_seg\[,-c(1:2,6,36:37)\]
S = scale(S, center=TRUE, scale=TRUE)

Extract the centers and scales from the rescaled data (which are named attributes)
==================================================================================

mu = attr(S,“scaled:center”) sigma = attr(S,“scaled:scale”)

\#Elbow Method for finding the optimal number of clusters

Compute and plot wss for k = 2 to k = 15.
=========================================

### probably 6 or 7

k.max &lt;- 15 wss &lt;- sapply(1:k.max, function(k){kmeans(S, k,
nstart=50,iter.max = 15 )$tot.withinss}) wss plot(1:k.max, wss,
type=“b”, pch = 19, frame = FALSE, xlab=“Number of clusters K”,
ylab=“Total within-clusters sum of squares”)

CH index
========

didnt converge in time –&gt; not sure how to use it
===================================================

N = nrow(S) CH\_grid = foreach(k = k\_grid, .combine = ‘c’)

for (k in 1:25) { cluster\_k = kmeans(S, k, nstart = 25) W =
cluster\_k*t**o**t*.*w**i**t**h**i**n**s**s**B* = *c**l**u**s**t**e**r*<sub>*k*</sub>betweenss
CH = (B/W) \* ((N-k) / (k-1)) print(CH) }

### gap statistic

users\_gap = clusGap(S, FUN = kmeans, nstart = 50, K.max = 10, B = 100)
plot(users\_gap)

Run k-means with 6 clusters and 25 starts
=========================================

25 different random initializations
===================================

picks the best of the 25 based on the SSEw
==========================================

clust1 = kmeans(S, 7, nstart=25)

What are the clusters?
======================

summary of a type of tweet that occurs again and again and again (data archetype)
=================================================================================

clust1$center \# not super helpful clust1$center\[1,\]*sigma + mu
clust1*c**e**n**t**e**r*\[2, \] \* *s**i**g**m**a* + *m**u**c**l**u**s**t*1center\[3,\]*sigma
+ mu
clust1*c**e**n**t**e**r*\[4, \] \* *s**i**g**m**a* + *m**u**c**l**u**s**t*1center\[5,\]*sigma
+ mu clust1$center\[6,\]*sigma + mu

Which users are in which clusters?
==================================

which(clust1*c**l**u**s**t**e**r* =  = 1)*w**h**i**c**h*(*c**l**u**s**t*1cluster
== 2)
which(clust1*c**l**u**s**t**e**r* =  = 3)*w**h**i**c**h*(*c**l**u**s**t*1cluster
== 4)
which(clust1*c**l**u**s**t**e**r* =  = 5)*w**h**i**c**h*(*c**l**u**s**t*1cluster
== 6)

A few plots with cluster membership shown
=========================================

qplot is in the ggplot2 library
===============================

qplot(photo\_sharing, travel, data=mkt\_seg,
color=factor(clust1*c**l**u**s**t**e**r*))*q**p**l**o**t*(*p**o**l**i**t**i**c**s*, *n**e**w**s*, *d**a**t**a* = *m**k**t*<sub>*s*</sub>*e**g*, *c**o**l**o**r* = *f**a**c**t**o**r*(*c**l**u**s**t*1cluster))

Using kmeans++ initialization
=============================

clust2 = kmeanspp(S, k=7, nstart=25)

clust2*c**e**n**t**e**r*\[1, \] \* *s**i**g**m**a* + *m**u**c**l**u**s**t*2center\[2,\]*sigma
+ mu clust2$center\[4,\]*sigma + mu

Which users are in which clusters?
==================================

which(clust2*c**l**u**s**t**e**r* =  = 1)*w**h**i**c**h*(*c**l**u**s**t*2cluster
== 2) which(clust2$cluster == 3)

Compare versus within-cluster average distances from the first run
==================================================================

ordinary k means sum of squares for each cluster
================================================

clust1$withinss \# k means++ sum of squares for each cluster clust2$withinss
\# sum of each clusters SSE for k means
sum(clust1$withinss) \# sum of each clusters SSE for k means++ , it does only SLIGHTLY better sum(clust2$withinss)

this does the sums for you
==========================

clust1*t**o**t*.*w**i**t**h**i**n**s**s**c**l**u**s**t*2tot.withinss

the between cluster measures SSEb between cluster sum of squares
================================================================

clust1*b**e**t**w**e**e**n**s**s**c**l**u**s**t*2betweenss

##### PCA

a quick heatmap visualization
=============================

ggcorrplot::ggcorrplot(cor(S))

looks a mess – reorder the variables by hierarchical clustering
===============================================================

ggcorrplot::ggcorrplot(cor(S), hc.order = TRUE)

##### Hierarchical Clustering stuff

Now the cars data
-----------------

mkt\_seg = read.csv(‘social\_marketing.csv’, header=TRUE)

\#scales the data and takes out the first column with the id number S =
mkt\_seg\[,-1\] S = scale(S, center=TRUE, scale=TRUE)

Extract the centers and scales from the rescaled data (which are named attributes)
==================================================================================

mu = attr(S,“scaled:center”) sigma = attr(S,“scaled:scale”)

First form a pairwise distance matrix
=====================================

distance\_between\_users = dist(S)

Now run hierarchical clustering
===============================

h1 = hclust(distance\_between\_users, method=‘complete’)

Cut the tree into 10 clusters
=============================

cluster1 = cutree(h1, k=5) summary(factor(cluster1))

Examine the cluster members
===========================

which(cluster1 == 1)

Plot the dendrogram
===================

plot(h1, cex=0.3)
