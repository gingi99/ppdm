# library
library(rlist)
source("~/R/ppdm/cluster.R")
source("~/R/ppdm/get_PPDM_Measure.R")

# k-memberクラスタリングの結果から、k匿名性を満たしたデータ(df)を作る
create_k_df_from_k_cluster <- function(k_clusters, 
                                       merged.size = "max", 
                                       rep.Method = "Merged"){
  list.df <- lapply(k_clusters, function(x){
    df.x <- list.stack(x)
    if(merged.size == "max"){
      return(getRepresentativeSample(df.x, size = nrow(df.x), method = rep.Method))
    }else{
      return(getRepresentativeSample(df.x, size = merged.size, method = rep.Method))
    }
  })
  return(list.stack(list.df))
}

# k-member クラスタリング
greedy_k_member_clustering <- function(df, k){
  df <- df.original
  if(nrow(df) <= k){
    warning("kより小さい数のデータです")
    return(df)
  }
  results <- list()
  while(nrow(df) >= k){
    ind.sample <- sample(1:nrow(df), 1, replace=F)
    ind.furthest.sample <- find_furthest_record(df, ind.sample)
    list.cluster <- list()
    list.cluster <- list.append(list.cluster, df[ind.furthest.sample,])
    df <- df[-ind.furthest.sample,]
    while(length(list.cluster) < k){
      ind.best.sample <- find_best_record(df, list.cluster)
      list.cluster <- list.append(list.cluster, df[ind.best.sample,])
      df <- df[-ind.best.sample,]
    }
    names(list.cluster) <- paste("member",1:length(list.cluster),sep="")
    results <- list.append(results, list.cluster)
  }
  names(results) <- paste("cluster",1:length(results),sep="")
  while(nrow(df) > 0){
    ind.sample <- sample(1:nrow(df), 1, replace=F)
    best.cluster <- find_best_cluster(results, df[ind.sample,])
    results <- aaa
    results[[best.cluster]] <- list.append(results[[best.cluster]], df[ind.sample,])
    names(results[[best.cluster]]) <- paste("member",1:length(results[[best.cluster]]),sep="")
    df <- df[-ind.sample,]
  }
  return(results)
}

# 指定したサンプル（index番号）とdfの中で最も距離の遠いサンプル（index番号）を返す。
# 距離関数は、dist_kusunoki。ゆくゆくは距離関数を自由に変更できるようにする。
find_furthest_record <- function(df, ind.sample){
  furthest_record <- 0
  list.df <- do.call(Zip, df)
  vec.dist <- sapply(list.df, function(record){
    target <- list.df[[ind.sample]]
    return(dist_kusunoki(target, record))
  })
  maxValue <- max(vec.dist)
  ind.maxValue <- which(vec.dist == maxValue)
  if(length(ind.maxValue) == 1){
    furthest_record <- ind.maxValue
  }else{
    furthest_record <- ind.maxValue[1]
  }
  return(furthest_record)
}

# 指定したclusterにdfの中で最も尤もらしいサンプル（レコード）を返す。
find_best_record <- function(df, list.cluster){
  best_record <- 0
  df.cluster <- list.stack(list.cluster)
  list.df <- do.call(Zip, df)
  vec.information.loss <- sapply(list.df, function(x){
    df.x <- list.stack(list(x))
    df.cx <- rbind(df.cluster, df.x)
    return(calInformationLoss(df.cx))
  })
  minValue <- min(vec.information.loss)
  ind.minValue <- which(vec.information.loss == minValue)
  if(length(ind.minValue) == 1){
    best_record <- ind.minValue
  }else{
    best_record <- ind.minValue[1]
  }
  return(best_record)
}

# 指定したサンプル（レコード）に最も入るべきクラスタを返す
find_best_cluster <- function(clusters, df.record){
  best_cluster <- ""
  n_cluster <- length(clusters)
  vec.information.loss <- sapply(clusters, function(x){
    df.x <- list.stack(x)
    df.cx <- rbind(df.x, df.record)
    return(calInformationLoss(df.cx))
  })
  minValue <- min(vec.information.loss)
  ind.minValue <- which(vec.information.loss == minValue)
  if(length(ind.minValue) == 1){
    best_cluster <- names(ind.minValue)
  }else{
    best_cluster <- names(ind.minValue[1])
  }
  return(best_cluster)
}
