library(stats)

#' Perform S3 Performance Benchmark for Put, Get, and List Operations
#'
#' @param foldername Character string specifying the folder to store benchmark results
#' @param number_samples Integer specifying the number of times to repeat each operation
#'
#' @return NULL (saves CSV files with benchmark results)
s3_performance_benchmark <- function(foldername, number_samples) {
  if (!dir.exists(foldername)) {
    dir.create(foldername, recursive = TRUE)
  }
  
  remote_folder = "benchmark_results"
  file_sizes <- c(10*1024, 100*1024, 1*1024*1024, 10*1024*1024, 100*1024*1024)

  put_results <- lapply(file_sizes, function(size) {
    local_file <- file.path(foldername, paste0("put_test_", size, "B.bin"))
    writeBin(raw(size), local_file)
    
    put_times <- sapply(1:number_samples, function(i) {
      remote_file <- paste0("put_", size, "B_", i, ".bin")
      start_time <- Sys.time()
      faasr_put_file(local_folder = foldername, 
                     local_file = basename(local_file), 
                     remote_folder = "benchmark", 
                     remote_file = remote_file)
      end_time <- Sys.time()
      as.numeric(end_time - start_time)
    })
    
    
    file.remove(local_file)
    
    data.frame(
      size = size,
      min = min(put_times),
      max = max(put_times),
      mean = mean(put_times),
      median = median(put_times),
      sd = sd(put_times)
    )
  })
  put_df <- do.call(rbind, put_results)
  write.csv(put_df, file.path(foldername, "put_benchmark.csv"), row.names = FALSE)
  
  faasr_put_file(local_folder = foldername, 
                 local_file = "put_benchmark.csv", 
                 remote_folder = remote_folder, 
                 remote_file = "put_benchmark.csv")
  
  # Get Operation Benchmark
  get_results <- lapply(file_sizes, function(size) {
    get_times <- sapply(1:number_samples, function(i) {
      remote_file <- paste0("put_", size, "B_", i, ".bin")
      local_file <- file.path(foldername, paste0("get_test_", size, "B_", i, ".bin"))
      
      start_time <- Sys.time()
      faasr_get_file(remote_folder = foldername, 
                     remote_file = remote_file, 
                     local_folder = foldername, 
                     local_file = basename(local_file))
      end_time <- Sys.time()
      
      file.remove(local_file)
      
      as.numeric(end_time - start_time)
    })
    
    data.frame(
      size = size,
      min = min(get_times),
      max = max(get_times),
      mean = mean(get_times),
      median = median(get_times),
      sd = sd(get_times)
    )
  })
  get_df <- do.call(rbind, get_results)
  write.csv(get_df, file.path(foldername, "get_benchmark.csv"), row.names = FALSE)
  
  faasr_put_file(local_folder = foldername, 
                 local_file = "get_benchmark.csv", 
                 remote_folder = remote_folder, 
                 remote_file = "get_benchmark.csv")
  
  list_sizes <- c(10, 100, 1000, 10000)
  list_results <- lapply(list_sizes, function(num_objects) {
    benchmark_folder <- paste0("benchmark/list_", num_objects, "/")
    
    existing_objects <- faasr_get_folder_list(faasr_prefix = benchmark_folder)
    
    
    if (length(existing_objects) == 0) {
      # Populate folder with small files (1KB each)
      for (i in 1:num_objects) {
        local_file <- file.path(foldername, paste0("small_", i, ".bin"))
        writeBin(raw(1024), local_file)
        faasr_put_file(local_folder = foldername, 
                       local_file = paste0("small_", i, ".bin"), 
                       remote_folder = benchmark_folder, 
                       remote_file = paste0("small_", i, ".bin"))
        file.remove(local_file)
      }
    }
    
    list_times <- sapply(1:number_samples, function(i) {
      start_time <- Sys.time()
      
  
      object_list <- faasr_get_folder_list(faasr_prefix = benchmark_folder)
      
      end_time <- Sys.time()
      as.numeric(end_time - start_time)
    })
    
    # Compute statistics
    data.frame(
      num_objects = num_objects,
      min = min(list_times),
      max = max(list_times),
      mean = mean(list_times),
      median = median(list_times),
      sd = sd(list_times)
    )
  })
  list_df <- do.call(rbind, list_results)
  write.csv(list_df, file.path(foldername, "list_benchmark.csv"), row.names = FALSE)
  
  faasr_put_file(local_folder = foldername, 
                 local_file = "list_benchmark.csv", 
                 remote_folder = remote_folder, 
                 remote_file = "list_benchmark.csv")
  invisible(NULL)
}