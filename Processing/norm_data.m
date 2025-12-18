% Function to normalize data (Z-score)
function norm_data = normalize_data(data)
    variance = std(data)^2;
    norm_data = (data - mean(data)) / sqrt(variance);
end