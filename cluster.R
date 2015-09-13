## cluster analysis for ppdm
library(matrixStats)
library(gdata)

## データを用意
#x <- iris[,1:4]

## グループ化
#km <- kmeans(x,10)

## グループ番号つける
#x$cluster <- km$cluster

## 各グループの各属性の値域で一般化
#y <- filter(x, cluster==1)
#y_min <- min(y[,1])
#y_max <- max(y[,1])
#y[,1] <- paste("[",y_min,",",y_max,"]",sep="")

## dist（なんらかの関数でサンプル間の距離を求める）
#dist.xx <- dist(iris[,1:4])

## 
#hc.ward.xx <- hclust(dist.xx, method = 'ward.D2')
#plot(hc.ward.xx, hang=-1)
#xx <- cmdscale(xx, k = ncol(iris[,1:4]))

#df.clust$height # 樹形図の枝の長さ
#df.clust$order # 対象の番号
#cutree(df.clust, k=49) # クラスタを49個作ったときに同じになる対象が分かる
#cor(df.dist2, cophenetic(df.clust)) # コーフェン行列との相関

#iris <- read_tsv("/data/uci/iris/iris.tsv")
#hayesroth <- read_tsv("/data/uci/hayes-roth/hayes-roth.tsv")
#df <- hayesroth
#df <- iris
#df <- ppCluster(df, merged.number=0, merged.rate=0.1, merged.size="max", rep.Method = "Merged")
# df <- ppCluster(df, merged.number=0, merged.rate=0.2, merged.size=2, rep.Method = "Merged")

## 楠木さんの距離関数
dist_kusunoki <- function(x, y){
  len <- length(x)
  ans <- 1.0
  for(i in 1:len){
    if(x[[i]] == "numeric"){
      v <- (x[[i]] - y[[i]])/(3.0 * sd)
      if(v > 1.0){
        v <- 1.0
      }
    }else{
      if(x[[i]] == y[[i]]){
        v <- 0.0
      }else{
        v <- 1.0
      }
    }
    ans <- ans + v
  }
  ans <- ans / len
  return(ans)
}

# データフレームをリストにする関数
Zip <- function(...) Map(list, ...)

# 代表点算出アルゴリズム
getRepresentativeSample <- function(df, size=1, method="Randomed"){
  if(method == "Randomed"){
    set.seed(1013)
    return(sample_n(sample_n(df, 1), size, replace=T))
  }else if(method == "Merged"){
    ans <- list()
    for(i in 1:ncol(df)){
      if(class(df[[i]]) == "numeric"){
        v <- mean(df[[i]])
        ans <- append(ans, v)
      }else{
        v <- sort(unique(df[[i]]))
        if(length(v) > 1){
          v <- paste(v, collapse=",")
          v <- paste("[",v,"]",sep="")
        }
        ans <- append(ans, v)
      }
    }
    ans <- sample_n(as.data.frame(ans), size, replace=T)
    setnames(ans, names(df))
    return(ans)
  }else{
    warning("no method")
    return(df)
  }
}

## クラスタリングによるデータ圧縮関数
ppCluster <- function(df, merged.number, merged.rate, merged.size = "max", rep.Method = "Randomed"){
  df.new <- tbl_df(data.frame())
  vec.classes      <-  select(df, ncol(df))[[1]]
  vec.classes.uniq <- unique(vec.classes)
  for(cl in vec.classes.uniq){
    print(cl)
    df[which(vec.classes == cl),] %>%
      select(1:(ncol(df)-1)) -> df.oneclass
    list.df.oneclass <- do.call(Zip, df.oneclass)    
    mat.df.dist <- matrix(0.0, nrow=nrow(df.oneclass), ncol=nrow(df.oneclass))
    for(i in 1:(nrow(df.oneclass)-1)){
      for(j in (i+1):nrow(df.oneclass)){
        x <- list.df.oneclass[[i]] 
        y <- list.df.oneclass[[j]]
        mat.df.dist[i,j] <- dist_kusunoki(x,y)
        mat.df.dist[j,i] <- dist_kusunoki(x,y)
      }
    }
    #　クラスター分析で距離が近いサンプルを決定
    clust.df <- hclust(as.dist(mat.df.dist), method = 'ward.D2')
    #plot(clust.df, hang=-1)
    
    # パラメータの数だけ、最も近いサンプル同士をマージしたものを作成
    df.merged <- tbl_df(data.frame())
    vec.targeted.sample <- vector()
    sample.number <- nrow(df.oneclass)
    # k個のクラスタを作る
    clust.k <- cutree(clust.df, k=round((sample.number - merged.number)*(1.0-merged.rate)))
    # 圧縮サンプルを削除
    df.oneclass.new <- df.oneclass[-which(duplicated2(clust.k)),]
    # どのクラスタに2つ以上サンプルがあるか
    vec.merged.cluster.number <- which(plyr::count(clust.k)$freq > 1)
    # 代表点を繰り返し追加する
    for(cn in vec.merged.cluster.number){
      vec.targets <- vector()
      for(cn2 in 1:length(clust.k)){
        if(cn == clust.k[cn2]){
          vec.targets <- append(vec.targets, cn2)
        }
      }
      vec.target.sample <- clust.df$order[vec.targets]
      df.target <- df.oneclass[vec.target.sample,]
      # 代表点求める
      if(merged.size == "max"){
        ans <- getRepresentativeSample(df.target, size = nrow(df.target), method = rep.Method)
      }else{
        ans <- getRepresentativeSample(df.target, size = merged.size, method = rep.Method)
      }
      ## 代表点を追加
      df.oneclass.new <- rbind(df.oneclass.new, ans)
      setnames(df.oneclass.new, names(df.oneclass))
    }
    df.new <- rbind_list(df.new, mutate(df.oneclass.new, cl))
  }
  setnames(df.new, names(df))
  return(df.new)
}

