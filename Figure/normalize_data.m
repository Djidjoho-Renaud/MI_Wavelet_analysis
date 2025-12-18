function normalized_data = normalize_data(data)
% NORMALIZE_DATA Normalize data using Z-score standardization
%   normalized_data = normalize_data(data)
%
%   This function performs Z-score normalization (standardization):
%   normalized_data = (data - mean) / standard_deviation
%
%   Input:
%       data - Vector of numerical data to normalize
%
%   Output:
%       normalized_data - Normalized data with mean=0 and std=1
%
%   Example:
%       x = [1, 2, 3, 4, 5];
%       x_norm = normalize_data(x);

    % Calculate variance
    var_data = std(data)^2;
    
    % Normalization: (x - mean) / standard deviation
    normalized_data = (data - mean(data)) / sqrt(var_data);
end