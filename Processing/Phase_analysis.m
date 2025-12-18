%% ========================================================================
%  PHASE ANGLE ANALYSIS
%  Computes phase angles and lags for WTC analysis
%  Sites: Nalohou (n) and Bellefoungou (b)
%  Conditions: Normal, Extreme, Deficient
%  Variables: NEE vs Tsoil (y) and Hsoil (z) at 2 depths
%% ========================================================================

%% ========================================================================
%  SECTION 1: DATA IMPORT AND PREPARATION
%% ========================================================================

% Import options configuration
opts = delimitedTextImportOptions("NumVariables", 8);
opts.DataLines = [2, Inf];
opts.Delimiter = ";";
opts.VariableNames = ["ID", "Day", "n", "Tsoil1", "Tsoil2", "Hsoil1", "Hsoil2", "NEE"];
opts.VariableTypes = ["double", "double", "double", "double", "double", "double", "double", "double"];
opts.ExtraColumnsRule = "ignore";
opts.EmptyLineRule = "read";

% Base paths
base_path_nal = ".\Table\RF\Nalohou\";
base_path_bel = ".\Table\RF\Bellefoungou\";

% Import Nalohou data
Nalohou_normal = readtable(base_path_nal + "Nal_night_rfxxx_normal_vers_.csv", opts);
Nalohou_deficient = readtable(base_path_nal + "Nal_night_rfxxx_deficient_vers_.csv", opts);
Nalohou_extreme = readtable(base_path_nal + "Nal_night_rfxxx_extreme_vers_.csv", opts);

% Import Bellefoungou data
Bellefoungou_normal = readtable(base_path_bel + "Bel_night_rfxxx_normal_.csv", opts);
Bellefoungou_deficient = readtable(base_path_bel + "Bel_night_rfxxx_deficient_.csv", opts);
Bellefoungou_extreme = readtable(base_path_bel + "Bel_night_rfxxx_extreme_vers_.csv", opts);

clear opts;

% Handle NaN values for Bellefoungou
Bellefoungou_normal.Hsoil2(isnan(Bellefoungou_normal.Hsoil2)) = 0;
Bellefoungou_deficient.Hsoil2(isnan(Bellefoungou_deficient.Hsoil2)) = 0;
Bellefoungou_extreme.Hsoil2(isnan(Bellefoungou_extreme.Hsoil2)) = 0;

%% ========================================================================
%  SECTION 2: HELPER FUNCTIONS
%% ========================================================================











%% ========================================================================
%  SECTION 3: DATA PREPARATION FOR ALL CONDITIONS
%% ========================================================================

% Prepare Nalohou data
[XNEE_normal_n, XToil_1_normal_n, XToil_2_normal_n, XHoil_1_normal_n, XHoil_2_normal_n, ...
 xtime_normal_n, ytime_1_normal_n, ytime_2_normal_n, ztime_1_normal_n, ztime_2_normal_n] = ...
 prepare_data(Nalohou_normal);

[XNEE_deficient_n, XToil_1_deficient_n, XToil_2_deficient_n, XHoil_1_deficient_n, XHoil_2_deficient_n, ...
 xtime_deficient_n, ytime_1_deficient_n, ytime_2_deficient_n, ztime_1_deficient_n, ztime_2_deficient_n] = ...
 prepare_data(Nalohou_deficient);

[XNEE_extreme_n, XToil_1_extreme_n, XToil_2_extreme_n, XHoil_1_extreme_n, XHoil_2_extreme_n, ...
 xtime_extreme_n, ytime_1_extreme_n, ytime_2_extreme_n, ztime_1_extreme_n, ztime_2_extreme_n] = ...
 prepare_data(Nalohou_extreme);

% Prepare Bellefoungou data
[XNEE_normal_b, XToil_1_normal_b, XToil_2_normal_b, XHoil_1_normal_b, XHoil_2_normal_b, ...
 xtime_normal_b, ytime_1_normal_b, ytime_2_normal_b, ztime_1_normal_b, ztime_2_normal_b] = ...
 prepare_data(Bellefoungou_normal);

[XNEE_deficient_b, XToil_1_deficient_b, XToil_2_deficient_b, XHoil_1_deficient_b, XHoil_2_deficient_b, ...
 xtime_deficient_b, ytime_1_deficient_b, ytime_2_deficient_b, ztime_1_deficient_b, ztime_2_deficient_b] = ...
 prepare_data(Bellefoungou_deficient);

[XNEE_extreme_b, XToil_1_extreme_b, XToil_2_extreme_b, XHoil_1_extreme_b, XHoil_2_extreme_b, ...
 xtime_extreme_b, ytime_1_extreme_b, ytime_2_extreme_b, ztime_1_extreme_b, ztime_2_extreme_b] = ...
 prepare_data(Bellefoungou_extreme);

%% ========================================================================
%  SECTION 4: COMPUTE PHASE ANGLES FOR NALOHOU
%% ========================================================================

fprintf('Computing phase angles for Nalohou...\n');

% Depth 1 - Temperature (y_1) and Moisture (z_1)
angles_normal_n_1 = compute_phase_angles(XNEE_normal_n, XToil_1_normal_n, XHoil_1_normal_n, ...
    xtime_normal_n, ytime_1_normal_n, ztime_1_normal_n, 'y_1', 'z_1', 1);

angles_extreme_n_1 = compute_phase_angles(XNEE_extreme_n, XToil_1_extreme_n, XHoil_1_extreme_n, ...
    xtime_extreme_n, ytime_1_extreme_n, ztime_1_extreme_n, 'y_1', 'z_1', 1);

angles_deficient_n_1 = compute_phase_angles(XNEE_deficient_n, XToil_1_deficient_n, XHoil_1_deficient_n, ...
    xtime_deficient_n, ytime_1_deficient_n, ztime_1_deficient_n, 'y_1', 'z_1', 1);

% Depth 2 - Temperature (y_2) and Moisture (z_2)
angles_normal_n_2 = compute_phase_angles(XNEE_normal_n, XToil_2_normal_n, XHoil_2_normal_n, ...
    xtime_normal_n, ytime_2_normal_n, ztime_2_normal_n, 'y_2', 'z_2', 2);

angles_extreme_n_2 = compute_phase_angles(XNEE_extreme_n, XToil_2_extreme_n, XHoil_2_extreme_n, ...
    xtime_extreme_n, ytime_2_extreme_n, ztime_2_extreme_n, 'y_2', 'z_2', 2);

angles_deficient_n_2 = compute_phase_angles(XNEE_deficient_n, XToil_2_deficient_n, XHoil_2_deficient_n, ...
    xtime_deficient_n, ytime_2_deficient_n, ztime_2_deficient_n, 'y_2', 'z_2', 2);

%% ========================================================================
%  SECTION 5: COMPUTE PHASE ANGLES FOR BELLEFOUNGOU
%% ========================================================================

fprintf('Computing phase angles for Bellefoungou...\n');

% Depth 1
angles_normal_b_1 = compute_phase_angles(XNEE_normal_b, XToil_1_normal_b, XHoil_1_normal_b, ...
    xtime_normal_b, ytime_1_normal_b, ztime_1_normal_b, 'y_1', 'z_1', 1);

angles_extreme_b_1 = compute_phase_angles(XNEE_extreme_b, XToil_1_extreme_b, XHoil_1_extreme_b, ...
    xtime_extreme_b, ytime_1_extreme_b, ztime_1_extreme_b, 'y_1', 'z_1', 1);

angles_deficient_b_1 = compute_phase_angles(XNEE_deficient_b, XToil_1_deficient_b, XHoil_1_deficient_b, ...
    xtime_deficient_b, ytime_1_deficient_b, ztime_1_deficient_b, 'y_1', 'z_1', 1);

% Depth 2
angles_normal_b_2 = compute_phase_angles(XNEE_normal_b, XToil_2_normal_b, XHoil_2_normal_b, ...
    xtime_normal_b, ytime_2_normal_b, ztime_2_normal_b, 'y_2', 'z_2', 2);

angles_extreme_b_2 = compute_phase_angles(XNEE_extreme_b, XToil_2_extreme_b, XHoil_2_extreme_b, ...
    xtime_extreme_b, ytime_2_extreme_b, ztime_2_extreme_b, 'y_2', 'z_2', 2);

angles_deficient_b_2 = compute_phase_angles(XNEE_deficient_b, XToil_2_deficient_b, XHoil_2_deficient_b, ...
    xtime_deficient_b, ytime_2_deficient_b, ztime_2_deficient_b, 'y_2', 'z_2', 2);

%% ========================================================================
%  SECTION 6: SAVE RESULTS TO CSV FILES
%% ========================================================================

fprintf('Saving results to CSV files...\n');

base_output = '.\Table\Phase\';

% Nalohou results
save_phase_table(angles_normal_n_1.z_1, angles_extreme_n_1.z_1, angles_deficient_n_1.z_1, ...
    [base_output 'Nalohou\Table_angle_nalohou_new_NeevsHs_1_new_category_precipitation_.csv']);

save_phase_table(angles_normal_n_1.y_1, angles_extreme_n_1.y_1, angles_deficient_n_1.y_1, ...
    [base_output 'Nalohou\Table_angle_nalohou_new_NeevsTs_1_new_category_precipitation_.csv']);

save_phase_table(angles_normal_n_2.z_2, angles_extreme_n_2.z_2, angles_deficient_n_2.z_2, ...
    [base_output  'Nalohou\Table_angle_nalohou_new_NeevsHs_2_new_category_precipitation_.csv']);

save_phase_table(angles_normal_n_2.y_2, angles_extreme_n_2.y_2, angles_deficient_n_2.y_2, ...
    [base_output  'Nalohou\Table_angle_nalohou_new_NeevsTs_2_new_category_precipitation_.csv']);

% Bellefoungou results
save_phase_table(angles_normal_b_1.z_1, angles_extreme_b_1.z_1, angles_deficient_b_1.z_1, ...
    [base_output 'Bellefoungou\Table_angle_bellefoungou_new_NeevsHs_1_new_category_precipitation_.csv']);

save_phase_table(angles_normal_b_1.y_1, angles_extreme_b_1.y_1, angles_deficient_b_1.y_1, ...
    [base_output  'Bellefoungou\Table_angle_bellefoungou_new_NeevsTs_1_new_category_precipitation_.csv']);

save_phase_table(angles_normal_b_2.z_2, angles_extreme_b_2.z_2, angles_deficient_b_2.z_2, ...
    [base_output 'Bellefoungou\Table_angle_bellefoungou_new_NeevsHs_2_new_category_precipitation_.csv']);

save_phase_table(angles_normal_b_2.y_2, angles_extreme_b_2.y_2, angles_deficient_b_2.y_2, ...
    [base_output 'Bellefoungou\Table_angle_bellefoungou_new_NeevsTs_2_new_category_precipitation_.csv']);

fprintf('Phase angle analysis complete!\n');

%% ========================================================================
%  END OF SCRIPT
%% ========================================================================