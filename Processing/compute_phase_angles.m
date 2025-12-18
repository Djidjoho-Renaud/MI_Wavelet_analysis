% Function to compute WTC and phase angles for a single condition
function [angles_struct] = compute_phase_angles(XNEE, var1, var2, xtime, var1_norm, var2_norm, ...
                                                 var1_name, var2_name, depth)
    % Perform WTC analysis
    [Rsq1, period1, ~, coi1, wtcsig1, ~, Wxy1] = wtc(XNEE, var1, 'MonteCarloCount', 1200);
    [Rsq2, period2, ~, coi2, wtcsig2, ~, Wxy2] = wtc(XNEE, var2, 'MonteCarloCount', 1200);
    
    % Perform global coherence test
    [~, ~, ~, period_g1, ~] = arcwisetest_globalcoher(xtime, var1_norm, ...
        'Dir', 'Vert', 'Alphap', 0.82:0.02:0.95, 'Arcsiglevel', 0.1, ...
        'Narc', 1200, 'MakeFigure', 0, 'Montepw', 10, 'Mother', 'Morlet');
    
    [~, ~, ~, period_g2, ~] = arcwisetest_globalcoher(xtime, var2_norm, ...
        'Dir', 'Vert', 'Alphap', 0.82:0.02:0.95, 'Arcsiglevel', 0.1, ...
        'Narc', 1200, 'MakeFigure', 0, 'Montepw', 10, 'Mother', 'Morlet');
    
    % Compute cone of influence masks
    incoi1 = period1(:) * (1 ./ coi1) > 1;
    incoi2 = period2(:) * (1 ./ coi2) > 1;
    
    % Period bins
    per = [4, 8, 16, 32, 64, 128, 256, 366];
    
    % Compute phase angles for variable 1
    angles1 = compute_angles_for_variable(Wxy1, Rsq1, incoi1, wtcsig1, period1, period_g1, per);
    
    % Compute phase angles for variable 2
    angles2 = compute_angles_for_variable(Wxy2, Rsq2, incoi2, wtcsig2, period2, period_g2, per);
    
    % Replace zeros with NaN
    angles1.Angle_rad(angles1.Angle_rad == 0) = NaN;
    angles2.Angle_rad(angles2.Angle_rad == 0) = NaN;
    
    % Store results in structure
    angles_struct.(var1_name) = angles1;
    angles_struct.(var2_name) = angles2;
end