%% ========================================================================
%  GLOBAL POWER ANALYSIS 
%  Script for analyzing coherence between NEE, soil temperature and 
%  soil moisture for two sites: Nalohou and Bellefoungou
%  Three conditions: Normal, Deficient, Extreme (Surplus)
%
%  REQUIRED FILES (must be in the same folder or MATLAB path):
%    - normalize_data.m
%    - plot_wtc_comparison.m
%    - plot_wtc_comparison_depth.m
%    - arcwisetest_globalpower.m
%Author: Renaud KOUKOUI
%email: renaud.koukoui@gmail.com
%% ========================================================================

%% ========================================================================
%  SECTION 1: CONFIGURATION AND DATA IMPORT
%% ========================================================================

% Import options configuration (common to all CSV files)
opts = delimitedTextImportOptions("NumVariables", 8);
opts.DataLines = [2, Inf];  % Skip header
opts.Delimiter = ";";
opts.VariableNames = ["ID", "Day", "n", "Tsoil1", "Tsoil2", "Hsoil1", "Hsoil2", "NEE"];
opts.VariableTypes = ["double", "double", "double", "double", "double", "double", "double", "double"];
opts.ExtraColumnsRule = "ignore";
opts.EmptyLineRule = "read";

% Base paths for data (adapt according to your file structure)
base_path_nal = "G:\DOSSIERS RENAUD\anon-ms-example\Carbone\New\Article_MI_version_0.5\Table\RF\Nalohou\";
base_path_bel = "G:\DOSSIERS RENAUD\anon-ms-example\Carbone\New\Article_MI_version_0.5\Table\RF\Bellefoungou\";

% Import data for Nalohou site
Nalohou_normal = readtable(base_path_nal + "Nal_night_rfxxx_normal_vers_161025.csv", opts);
Nalohou_deficient = readtable(base_path_nal + "Nal_night_rfxxx_deficient_vers_161025.csv", opts);
Nalohou_extreme = readtable(base_path_nal + "Nal_night_rfxxx_extreme_vers_161025.csv", opts);

% Import data for Bellefoungou site
Bellefoungou_normal = readtable(base_path_bel + "Bel_night_rfxxx_normal_161025.csv", opts);
Bellefoungou_deficient = readtable(base_path_bel + "Bel_night_rfxxx_deficient_161025.csv", opts);
Bellefoungou_extreme = readtable(base_path_bel + "Bel_night_rfxxx_extreme_vers_161025.csv", opts);

clear opts base_path_nal base_path_bel;

% Handle NaN values for Bellefoungou (Hsoil2 variable)
Bellefoungou_normal.Hsoil2(isnan(Bellefoungou_normal.Hsoil2)) = 0;
Bellefoungou_deficient.Hsoil2(isnan(Bellefoungou_deficient.Hsoil2)) = 0;
Bellefoungou_extreme.Hsoil2(isnan(Bellefoungou_extreme.Hsoil2)) = 0;

%% ========================================================================
%  SECTION 2: DATA PREPARATION FOR NALOHOU SITE
%% ========================================================================

% --- NORMAL CONDITION ---
% Extraction and normalization for Nalohou Normal
xtime_normal_n = normalize_data(Nalohou_normal.NEE);      % Net Ecosystem Exchange
ytime_1_normal_n = normalize_data(Nalohou_normal.Tsoil1); % Soil Temperature depth 1
ytime_2_normal_n = normalize_data(Nalohou_normal.Tsoil2); % Soil Temperature depth 2
ztime_1_normal_n = normalize_data(Nalohou_normal.Hsoil1); % Soil Moisture depth 1
ztime_2_normal_n = normalize_data(Nalohou_normal.Hsoil2); % Soil Moisture depth 2

% --- DEFICIENT CONDITION ---
% Extraction and normalization for Nalohou Deficient
xtime_deficient_n = normalize_data(Nalohou_deficient.NEE);
ytime_1_deficient_n = normalize_data(Nalohou_deficient.Tsoil1);
ytime_2_deficient_n = normalize_data(Nalohou_deficient.Tsoil2);
ztime_1_deficient_n = normalize_data(Nalohou_deficient.Hsoil1);
ztime_2_deficient_n = normalize_data(Nalohou_deficient.Hsoil2);

% --- EXTREME CONDITION (SURPLUS) ---
% Extraction and normalization for Nalohou Extreme
xtime_extreme_n = normalize_data(Nalohou_extreme.NEE);
ytime_1_extreme_n = normalize_data(Nalohou_extreme.Tsoil1);
ytime_2_extreme_n = normalize_data(Nalohou_extreme.Tsoil2);
ztime_1_extreme_n = normalize_data(Nalohou_extreme.Hsoil1);
ztime_2_extreme_n = normalize_data(Nalohou_extreme.Hsoil2);

%% ========================================================================
%  SECTION 3: DATA PREPARATION FOR BELLEFOUNGOU SITE
%% ========================================================================

% --- NORMAL CONDITION ---
% Extraction and normalization for Bellefoungou Normal
xtime_normal_b = normalize_data(Bellefoungou_normal.NEE);
ytime_1_normal_b = normalize_data(Bellefoungou_normal.Tsoil1);
ytime_2_normal_b = normalize_data(Bellefoungou_normal.Tsoil2);
ztime_1_normal_b = normalize_data(Bellefoungou_normal.Hsoil1);
ztime_2_normal_b = normalize_data(Bellefoungou_normal.Hsoil2);

% --- DEFICIENT CONDITION ---
% Extraction and normalization for Bellefoungou Deficient
xtime_deficient_b = normalize_data(Bellefoungou_deficient.NEE);
ytime_1_deficient_b = normalize_data(Bellefoungou_deficient.Tsoil1);
ytime_2_deficient_b = normalize_data(Bellefoungou_deficient.Tsoil2);
ztime_1_deficient_b = normalize_data(Bellefoungou_deficient.Hsoil1);
ztime_2_deficient_b = normalize_data(Bellefoungou_deficient.Hsoil2);

% --- EXTREME CONDITION (SURPLUS) ---
% Extraction and normalization for Bellefoungou Extreme
xtime_extreme_b = normalize_data(Bellefoungou_extreme.NEE);
ytime_1_extreme_b = normalize_data(Bellefoungou_extreme.Tsoil1);
ytime_2_extreme_b = normalize_data(Bellefoungou_extreme.Tsoil2);
ztime_1_extreme_b = normalize_data(Bellefoungou_extreme.Hsoil1);
ztime_2_extreme_b = normalize_data(Bellefoungou_extreme.Hsoil2);

%% ========================================================================
%  SECTION 4: FIGURE 7 - ANALYSIS AT FIRST DEPTH
%% ========================================================================

rng(1589);  % Set random seed for reproducibility

figure('Name', 'WTC - First Depth Analysis - Nalohou & Bellefoungou');
t = tiledlayout(3, 3, 'TileSpacing', 'Compact', 'Padding', 'Compact');

% --- ROW 1: NEE (Net Ecosystem Exchange) ---
nexttile; 
plot_wtc_comparison(xtime_normal_n, xtime_normal_b, 'Normal', ...
    'NEE-GP(\mu mol^{2} m^{-4} s^{-2})', 'a)', false);

nexttile; 
plot_wtc_comparison(xtime_deficient_n, xtime_deficient_b, 'Deficient', '', 'b)', false);

nexttile; 
plot_wtc_comparison(xtime_extreme_n, xtime_extreme_b, 'Surplus', '', 'c)', true);

% --- ROW 2: SOIL MOISTURE (1st depth) ---
nexttile; 
plot_wtc_comparison_depth(ztime_1_normal_n, ztime_1_normal_b, '', ...
    '\theta-GP(cm^{6} cm^{-6})', 'd)', '1^{st} depth', false);

nexttile; 
plot_wtc_comparison_depth(ztime_1_deficient_n, ztime_1_deficient_b, '', '', 'e)', '1^{st} depth', false);

nexttile; 
plot_wtc_comparison_depth(ztime_1_extreme_n, ztime_1_extreme_b, '', '', 'f)', '1^{st} depth', true);

% --- ROW 3: SOIL TEMPERATURE (1st depth) ---
nexttile; 
plot_wtc_comparison_depth(ytime_1_normal_n, ytime_1_normal_b, '', ...
    'Tsoil-GP(^\circC^2)', 'g)', '1^{st} depth', false);
xlabel('Periods(night)');

nexttile; 
plot_wtc_comparison_depth(ytime_1_deficient_n, ytime_1_deficient_b, '', '', 'h)', '1^{st} depth', false);
xlabel('Periods(night)');

nexttile; 
plot_wtc_comparison_depth(ytime_1_extreme_n, ytime_1_extreme_b, '', '', 'i)', '1^{st} depth', true);
xlabel('Periods(night)');

%% ========================================================================
%  SECTION 5: FIGURE S3 - ANALYSIS AT SECOND DEPTH
%% ========================================================================

rng(1589);  % Set random seed for reproducibility

figure('Name', 'WTC - Second Depth Analysis - Nalohou & Bellefoungou');
t = tiledlayout(3, 3, 'TileSpacing', 'Compact', 'Padding', 'Compact');

% --- ROW 1: NEE (Net Ecosystem Exchange) ---
nexttile; 
plot_wtc_comparison(xtime_normal_n, xtime_normal_b, 'Normal', ...
    'NEE-GP(\mu mol^{2} m^{-4} s^{-2})', 'a)', false);

nexttile; 
plot_wtc_comparison(xtime_deficient_n, xtime_deficient_b, 'Deficient', '', 'b)', false);

nexttile; 
plot_wtc_comparison(xtime_extreme_n, xtime_extreme_b, 'Surplus', '', 'c)', true);

% --- ROW 2: SOIL MOISTURE (2nd depth) ---
nexttile; 
plot_wtc_comparison_depth(ztime_2_normal_n, ztime_2_normal_b, '', ...
    '\theta-GP(cm^{6} cm^{-6})', 'd)', '2^{nd} depth', false);

nexttile; 
plot_wtc_comparison_depth(ztime_2_deficient_n, ztime_2_deficient_b, '', '', 'e)', '2^{nd} depth', false);

nexttile; 
plot_wtc_comparison_depth(ztime_2_extreme_n, ztime_2_extreme_b, '', '', 'f)', '2^{nd} depth', true);

% --- ROW 3: SOIL TEMPERATURE (2nd depth) ---
nexttile; 
plot_wtc_comparison_depth(ytime_2_normal_n, ytime_2_normal_b, '', ...
    'Tsoil-GP(^\circC^2)', 'g)', '2^{nd} depth', false);
xlabel('Periods(night)');

nexttile; 
plot_wtc_comparison_depth(ytime_2_deficient_n, ytime_2_deficient_b, '', '', 'h)', '2^{nd} depth', false);
xlabel('Periods(night)');

nexttile; 
plot_wtc_comparison_depth(ytime_2_extreme_n, ytime_2_extreme_b, '', '', 'i)', '2^{nd} depth', true);
xlabel('Periods(night)');

%% ========================================================================
%  END OF SCRIPT
%% ========================================================================