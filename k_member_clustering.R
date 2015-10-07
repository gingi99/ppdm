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
    df <- df[-ind.furthest.sample,]
    list.cluster <- list()
    list.cluster <- list.append(list.cluster, df[ind.furthest.sample,])
    while(length(list.cluster) < k){
      ind.best.sample <- find_best_record(df, list.cluster)
      df <- df[-ind.best.sample,]
      list.cluster <- list.append(list.cluster, df[ind.best.sample,])
    }
    names(list.cluster) <- paste("member",1:length(list.cluster),sep="")
    results <- list.append(results, list.cluster)
  }
  names(results) <- paste("cluster",1:length(results),sep="")
  # ここから
  while(nrow(df) > 0){
    ind.sample <- sample(1:nrow(df), 1, replace=F)
    df <- df[-ind.sample,]
    best.cluster <- find_best_cluster(results, ind.sample)
    #list.map(results, cluster3)
  }
  return(results)
}

# 指定したサンプル（index番号）とdfの中で最も距離の遠いサンプル（index番号）を返す。
# 距離関数は、dist_kusunoki。ゆくゆくは距離関数を自由に変更できるようにする。
find_furthest_record <- function(df, ind.sample){
  furthest_record <- 0
  source("~/R/ppdm/cluster.R")
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
  source("~/R/ppdm/cluster.R")
  list.df <- do.call(Zip, df)
  vec.information.loss <- sapply(list.df, function(x){
    df.x <- list.stack(list(x))
    df.cx <- rbind(df.cluster, df.x)
    source("~/R/ppdm/get_PPDM_Measure.R")
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
find_best_cluster <- function(clusters, record){
  best_cluster <- ""
  
  return(best_cluster)
}
