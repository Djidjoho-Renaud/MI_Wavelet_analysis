% Function to save phase angle results to CSV
function save_phase_table(angles_normal, angles_extreme, angles_deficient, filename)
    % Concatenate data from all conditions
    Year = [repmat("Normal", 1, length(angles_normal.Angle_period)), ...
            repmat("Extreme", 1, length(angles_extreme.Angle_period)), ...
            repmat("Deficient", 1, length(angles_deficient.Angle_period))];
    
    Period = [angles_normal.Angle_period, angles_extreme.Angle_period, angles_deficient.Angle_period];
    Phase = [angles_normal.Angle_rad, angles_extreme.Angle_rad, angles_deficient.Angle_rad];
    Phase_coi = [angles_normal.Angle_coi, angles_extreme.Angle_coi, angles_deficient.Angle_coi];
    Lags = [angles_normal.Angle_lag, angles_extreme.Angle_lag, angles_deficient.Angle_lag];
    Lags_coi = [angles_normal.Angle_lag_coi, angles_extreme.Angle_lag_coi, angles_deficient.Angle_lag_coi];
    
    % Create table
    result_table = table(Year', Period', Phase', Phase_coi', Lags', Lags_coi', ...
                         'VariableNames', {'Years', 'Period', 'Phase', 'Phase_coi', 'Lags', 'Lags_coi'});
    
    % Write to CSV
    writetable(result_table, filename);
end
