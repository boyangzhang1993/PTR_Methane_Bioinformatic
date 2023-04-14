# Split data by cross validation
split_data <- function(candidate_data) {
  spilter.data = candidate_data
  spilter.data$CV_ID <- c(1:nrow(spilter.data))
  n = nrow(spilter.data)
  lb.testing.dataset.size = floor(n / 5)
  ub.testing.dataset.size = ceiling(n / 5)
  id_data <- spilter.data$CV_ID
  i_count_cv_group = 0
  ans <- c()
  while (length(ans) < lb.testing.dataset.size) {
    a = sample(id_data, size = 1)
    b = spilter.data[a, ]$ANIMID
    sample_number = spilter.data[which(spilter.data$ANIMID == b), ]$CV_ID
    unique_check = length(which(ans %in% sample_number))
    length_check = length(ans) + length(sample_number)
    if (unique_check == 0 &&
        length_check <= ub.testing.dataset.size) {
      ans <- c(ans, sample_number)
    }
  }
  init_ans = ans
  length(ans)
  while (i_count_cv_group < 4) {
    ans <- c()
    i_count_cv_group = i_count_cv_group + 1
    
    while (length(ans) < lb.testing.dataset.size) {
      a = sample(id_data, size = 1)
      b = spilter.data[a, ]$ANIMID
      sample_number = spilter.data[which(spilter.data$ANIMID == b), ]$CV_ID
      unique_check = length(which(ans %in% sample_number))
      length_check = length(ans) + length(sample_number)
      if (unique_check == 0 &&
          length_check <= ub.testing.dataset.size) {
        ans <- c(ans, sample_number)
      }
    }
    if (length(ans) == 21) {
      ans <- c(ans, NA)
    }
    init_ans = rbind(init_ans, ans)
    id_data <- id_data[-which(id_data %in% ans)]
  }
  
  ans_collection = init_ans
  return(ans_collection)
}
