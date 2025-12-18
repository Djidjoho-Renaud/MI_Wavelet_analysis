% Function to compute angles for a single variable
function angles = compute_angles_for_variable(Wxy_full, Rsq_full, incoi_full, wtcsig_full, ...
                                               period_full, period_global, per)
    % Initialize output arrays
    n_periods = length(per) - 1;
    Angle_mean = nan(1, n_periods);
    Angle_coi = nan(1, n_periods);
    Angle_rad = nan(1, n_periods);
    Angle_str = nan(1, n_periods);
    Angle_period = nan(1, n_periods);
    Angle_lag = nan(1, n_periods);
    Angle_lag_coi = nan(1, n_periods);
    
    for n = 1:n_periods
        % Find indices for current period range
        indices = find(period_global >= per(n) & period_global < per(n+1));
        
        if isempty(indices)
            continue;
        end
        
        % Extract data for current period range
        Wxy = Wxy_full(indices, :);
        Rsq = Rsq_full(indices, :);
        Coi = incoi_full(indices, :);
        wtcsig = wtcsig_full(indices, :);
        period = period_full(indices);
        
        % Extract significant angles outside COI with Rsq >= 0.2
        [nbLignes, nbColonnes] = size(Wxy);
        Wxy_significant = [];
        
        for i = 1:nbLignes
            for j = 1:nbColonnes
                if Coi(i,j) == 0 && wtcsig(i,j) >= 1 && Rsq(i,j) >= 0.2
                    Wxy_significant = [Wxy_significant, Wxy(i,j)];
                end
            end
        end
        
        % Compute angle statistics
        if isempty(Wxy_significant)
            meantheta = NaN;
            anglestrength = NaN;
            confangle = NaN;
        else
            [meantheta, anglestrength, ~, confangle] = anglemean(angle(Wxy_significant));
        end
        
        % Store results
        Angle_mean(n) = rad2deg(mod(meantheta, 2*pi));
        Angle_coi(n) = confangle;
        Angle_rad(n) = meantheta;
        Angle_str(n) = anglestrength;
        Angle_period(n) = mean(period);
        Angle_lag(n) = Angle_rad(n) * Angle_period(n) / (2*pi);
        Angle_lag_coi(n) = Angle_coi(n) * Angle_period(n) / (2*pi);
    end
    
    % Return structure
    angles.Angle_mean = Angle_mean;
    angles.Angle_coi = Angle_coi;
    angles.Angle_rad = Angle_rad;
    angles.Angle_str = Angle_str;
    angles.Angle_period = Angle_period;
    angles.Angle_lag = Angle_lag;
    angles.Angle_lag_coi = Angle_lag_coi;
end