## 決定表が満たしている匿名性
calKAnonymity <- function(decision.table){
  decIdx <- ncol(decision.table)
  decision.table <- as.data.frame(decision.table)
  clsVec <- decision.table[,decIdx]
  uniqueCls <- unique(clsVec)
  min.count.uniq <- 99999999
  for(dec.cls in uniqueCls){
    tmp.decision.table <- decision.table[which(clsVec==dec.cls),]      
    num <- nrow(tmp.decision.table)
    vec <- vector()
    for(i in 1:num){
      vec.tmp <- paste(as.vector(as.matrix(tmp.decision.table[i,])), collapse="+")
      vec <- append(vec, vec.tmp)
    }
    count.uniq <- unname(table(vec))
    print(count.uniq)
    if(min(count.uniq) < min.count.uniq){
      min.count.uniq <- min(count.uniq)
    }
  }
  return(min.count.uniq)
}

## （質的データの）エントロピー（平均情報量）の算出
calEntropyForVec <- function(vec){
   entropy <- 0
   uniqueVec <- unique(vec)
   for(uv in uniqueVec){
     p <- length(which(vec == uv)) / length(vec)
     entropy <- entropy + (-1 * p * log2(p))
   }
   return(entropy)
}

## データのエントロピー（平均情報量）の算出
## 1レコード（1対象）を1要素として、計算
calEntropyForData <- function(data){
  entropy <- 0
  num <- nrow(data) 
  vec <- vector()
  for(i in 1:num){
    vec.tmp <- paste(as.vector(as.matrix(data[i,])), collapse="+")
    vec <- append(vec, vec.tmp)
  }
  count.uniq <- unname(table(vec))
  print(count.uniq)
  for(j in 1:length(count.uniq)){
    entropy <- entropy + -(j / num) * log2(j / num) 
  }
  #print(entropy)
  return(entropy)
}

## データのエントロピー（平均情報量）の算出
## 1セル（1属性値）を1要素として、計算
calEntropyForElement <- function(data){
  entropy <- 0
  num <- nrow(decision.table) 
  for(i in 1:ncol(decision.table)){
    vec <- c(decision.table[,i])
    vec.uniq <- unique(vec)
    count.uniq <- unname(table(vec))
    for(j in 1:length(count.uniq)){
      entropy <- entropy + -(j / num) * log2(j / num) 
    }
  }
  print(entropy)
  return(entropy)
}

## Dissimilarity Measure (by Oliveria and Zaiane)
calDissimilarity <- function(ori_data, ano_data){
  dissimilarity <- 0
  num <- nrow(ori_data)
  ori_vec <- vector(); ano_vec <- vector()
  for(i in 1:num){
    vec.tmp <- paste(as.vector(as.matrix(ori_data[i,])), collapse="+")
    ori_vec <- append(ori_vec, vec.tmp)
    vec.tmp <- paste(as.vector(as.matrix(ano_data[i,])), collapse="+")
    ano_vec <- append(ano_vec, vec.tmp)
  }
  for(i in 1:length(ori_vec)){
    dissimilarity <- dissimilarity +
                     (sum(ano_vec == ano_vec[i]) - sum(ori_vec == ori_vec[i])) / sum(ano_vec == ano_vec[i])
  }
  return(dissimilarity)
}

## Loss Metric (by Vijay)
## 途中
calLossMetric <- function(ori_data, ano_data){
  lossMetric <- 0
  num.samples <- nrow(ori_data)
  num.attr <- ncol(ori_data) - 1
  for(i in 1:num.attr){
    table(ori_data[i])
  }
}

## Classification Metric (by Vijay)
calClassificationMetric <- function(ans_ori, ans_ano){
  return(sum(ans_ori == ans_ano) /  length(ans_ori))
}

## Discernibility Metric (by Agrawal)
## 理解中

## Minimal Distorion Metrics：MD（by ？？）
calMD <- function(ori_data, ano_data){
  md <- 0
  for(i in 1:(ncol(ori_data)-1)){
    ori_vec <- ori_data[i]
    ano_vec <- ano_data[i]
    md <- md + sum(ori_vec == ano_vec)
  }
  return(md)  
}

## Information Metric：ILoss (by ??)
calInformationMetrics <- function(ori_vec, ano_vec){
   return((length(unique(ano_vec)) - 1) / length(unique(ori_vec)))
}

