%% ========================================================================
%  WAVELET TRANSFORM COHERENCE (WTC) ANALYSIS
%  Coherence analysis between NEE, soil temperature and soil moisture
%  Sites: Nalohou and Bellefoungou
%  Conditions: Normal, Deficient, Surplus (Extreme)
%  Depths: 1st and 2nd depth
%
%  REQUIRED FUNCTIONS:
%    - wtc.m (Wavelet Transform Coherence)
%    - arcwisetest_globalcoher.m (Global coherence test)
%    - normalize_data.m
%     -create_wtc_subplot.m
%     -create_coherence_subplot.m
%     -create_site_depth_figure.m
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
base_path_nal = ".\Table\RF\Nalohou\";
base_path_bel = ".\Table\RF\Bellefoungou\";

% Import data for Nalohou site
Nalohou_normal = readtable(base_path_nal + "Nal_night_rfxxx_normal_vers_.csv", opts);
Nalohou_deficient = readtable(base_path_nal + "Nal_night_rfxxx_deficient_vers_.csv", opts);
Nalohou_extreme = readtable(base_path_nal + "Nal_night_rfxxx_extreme_vers_.csv", opts);

% Import data for Bellefoungou site
Bellefoungou_normal = readtable(base_path_bel + "Bel_night_rfxxx_normal_.csv", opts);
Bellefoungou_deficient = readtable(base_path_bel + "Bel_night_rfxxx_deficient_.csv", opts);
Bellefoungou_extreme = readtable(base_path_bel + "Bel_night_rfxxx_extreme_vers_.csv", opts);

clear opts base_path_nal base_path_bel;

% Handle NaN values for Bellefoungou (Hsoil2 variable)
Bellefoungou_normal.Hsoil2(isnan(Bellefoungou_normal.Hsoil2)) = 0;
Bellefoungou_deficient.Hsoil2(isnan(Bellefoungou_deficient.Hsoil2)) = 0;
Bellefoungou_extreme.Hsoil2(isnan(Bellefoungou_extreme.Hsoil2)) = 0;

%% ========================================================================
%  SECTION 2: DATA PREPARATION FOR NALOHOU SITE
%% ========================================================================

% --- NORMAL CONDITION ---
XNEE_normal_n=Nalohou_normal.NEE;
XToil_1_normal_n=Nalohou_normal.Tsoil1;
XToil_2_normal_n=Nalohou_normal.Tsoil2;
XHoil_1_normal_n=Nalohou_normal.Hsoil1;
XHoil_2_normal_n=Nalohou_normal.Hsoil2;
% Extraction and normalization for Nalohou Normal
xtime_normal_n = normalize_data(Nalohou_normal.NEE);      % Net Ecosystem Exchange
ytime_1_normal_n = normalize_data(Nalohou_normal.Tsoil1); % Soil Temperature depth 1
ytime_2_normal_n = normalize_data(Nalohou_normal.Tsoil2); % Soil Temperature depth 2
ztime_1_normal_n = normalize_data(Nalohou_normal.Hsoil1); % Soil Moisture depth 1
ztime_2_normal_n = normalize_data(Nalohou_normal.Hsoil2); % Soil Moisture depth 2

% --- DEFICIENT CONDITION ---
XNEE_deficient_n=Nalohou_deficient.NEE;
XToil_1_deficient_n=Nalohou_deficient.Tsoil1;
XToil_2_deficient_n=Nalohou_deficient.Tsoil2;
XHoil_1_deficient_n=Nalohou_deficient.Hsoil1;
XHoil_2_deficient_n=Nalohou_deficient.Hsoil2;
% Extraction and normalization for Nalohou Deficient
xtime_deficient_n = normalize_data(Nalohou_deficient.NEE);
ytime_1_deficient_n = normalize_data(Nalohou_deficient.Tsoil1);
ytime_2_deficient_n = normalize_data(Nalohou_deficient.Tsoil2);
ztime_1_deficient_n = normalize_data(Nalohou_deficient.Hsoil1);
ztime_2_deficient_n = normalize_data(Nalohou_deficient.Hsoil2);

% --- EXTREME CONDITION (SURPLUS) ---
XNEE_extreme_n=Nalohou_extreme.NEE;
XToil_1_extreme_n=Nalohou_extreme.Tsoil1;
XToil_2_extreme_n=Nalohou_extreme.Tsoil2;
XHoil_1_extreme_n=Nalohou_extreme.Hsoil1;
XHoil_2_extreme_n=Nalohou_extreme.Hsoil2;
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
XNEE_normal_b=Bellefoungou_normal.NEE;
XToil_1_normal_b=Bellefoungou_normal.Tsoil1;
XToil_2_normal_b=Bellefoungou_normal.Tsoil2;
XHoil_1_normal_b=Bellefoungou_normal.Hsoil1;
XHoil_2_normal_b=Bellefoungou_normal.Hsoil2;
% Extraction and normalization for Bellefoungou Normal
xtime_normal_b = normalize_data(Bellefoungou_normal.NEE);
ytime_1_normal_b = normalize_data(Bellefoungou_normal.Tsoil1);
ytime_2_normal_b = normalize_data(Bellefoungou_normal.Tsoil2);
ztime_1_normal_b = normalize_data(Bellefoungou_normal.Hsoil1);
ztime_2_normal_b = normalize_data(Bellefoungou_normal.Hsoil2);

% --- DEFICIENT CONDITION ---
XNEE_deficient_b=Bellefoungou_deficient.NEE;
XToil_1_deficient_b=Bellefoungou_deficient.Tsoil1;
XToil_2_deficient_b=Bellefoungou_deficient.Tsoil2;
XHoil_1_deficient_b=Bellefoungou_deficient.Hsoil1;
XHoil_2_deficient_b=Bellefoungou_deficient.Hsoil2;
% Extraction and normalization for Bellefoungou Deficient
xtime_deficient_b = normalize_data(Bellefoungou_deficient.NEE);
ytime_1_deficient_b = normalize_data(Bellefoungou_deficient.Tsoil1);
ytime_2_deficient_b = normalize_data(Bellefoungou_deficient.Tsoil2);
ztime_1_deficient_b = normalize_data(Bellefoungou_deficient.Hsoil1);
ztime_2_deficient_b = normalize_data(Bellefoungou_deficient.Hsoil2);

% --- EXTREME CONDITION (SURPLUS) ---
XNEE_extreme_b=Bellefoungou_extreme.NEE;
XToil_1_extreme_b=Bellefoungou_extreme.Tsoil1;
XToil_2_extreme_b=Bellefoungou_extreme.Tsoil2;
XHoil_1_extreme_b=Bellefoungou_extreme.Hsoil1;
XHoil_2_extreme_b=Bellefoungou_extreme.Hsoil2;

% Extraction and normalization for Bellefoungou Extreme
xtime_extreme_b = normalize_data(Bellefoungou_extreme.NEE);
ytime_1_extreme_b = normalize_data(Bellefoungou_extreme.Tsoil1);
ytime_2_extreme_b = normalize_data(Bellefoungou_extreme.Tsoil2);
ztime_1_extreme_b = normalize_data(Bellefoungou_extreme.Hsoil1);
ztime_2_extreme_b = normalize_data(Bellefoungou_extreme.Hsoil2);
%% ========================================================================
%  GLOBAL SETTINGS
%% ========================================================================

% Gray color for confidence lines
grayColor = [.7 .7 .7];

% Set random seed for reproducibility
rng(1589);

% Common settings for all WTC plots
monthLabels = {'J', 'F', 'Ma', 'Ap', 'M', 'J', 'Jl', 'A', 'S', 'O', 'N', 'D'};
monthTicks = 30:30:366;
commonOptions = {'MonteCarloCount', 1200, 'MaxScale', 128};

% Arc colors and styles for coherence plots
arc_colors = {'#228C22', '#FF4500'}; % Dark green for θ, Orange-red for T
arc_styles = {'-', '--'};            % Solid line for θ, dashed for T

%% ========================================================================
%  MAIN EXECUTION: Generate all figures
%% ========================================================================

% ===== FIGURE 8: NALOHOU - DEPTH 1 =====
create_site_depth_figure('Nalohou', 1, ...
    XNEE_normal_n, XHoil_1_normal_n, XToil_1_normal_n, ...
    XNEE_extreme_n, XHoil_1_extreme_n, XToil_1_extreme_n, ...
    XNEE_deficient_n, XHoil_1_deficient_n, XToil_1_deficient_n, ...
    xtime_normal_n, ztime_1_normal_n, ytime_1_normal_n, ...
    xtime_extreme_n, ztime_1_extreme_n, ytime_1_extreme_n, ...
    xtime_deficient_n, ztime_1_deficient_n, ytime_1_deficient_n, ...
    commonOptions, monthTicks, monthLabels, grayColor, arc_colors, arc_styles);

% ===== FIGURE S4: NALOHOU - DEPTH 2 =====
create_site_depth_figure('Nalohou', 2, ...
    XNEE_normal_n, XHoil_2_normal_n, XToil_2_normal_n, ...
    XNEE_extreme_n, XHoil_2_extreme_n, XToil_2_extreme_n, ...
    XNEE_deficient_n, XHoil_2_deficient_n, XToil_2_deficient_n, ...
    xtime_normal_n, ztime_2_normal_n, ytime_2_normal_n, ...
    xtime_extreme_n, ztime_2_extreme_n, ytime_2_extreme_n, ...
    xtime_deficient_n, ztime_2_deficient_n, ytime_2_deficient_n, ...
    commonOptions, monthTicks, monthLabels, grayColor, arc_colors, arc_styles);

% ===== FIGURE 9: BELLEFOUNGOU - DEPTH 1 =====
create_site_depth_figure('Bellefoungou', 1, ...
    XNEE_normal_b, XHoil_1_normal_b, XToil_1_normal_b, ...
    XNEE_extreme_b, XHoil_1_extreme_b, XToil_1_extreme_b, ...
    XNEE_deficient_b, XHoil_1_deficient_b, XToil_1_deficient_b, ...
    xtime_normal_b, ztime_1_normal_b, ytime_1_normal_b, ...
    xtime_extreme_b, ztime_1_extreme_b, ytime_1_extreme_b, ...
    xtime_deficient_b, ztime_1_deficient_b, ytime_1_deficient_b, ...
    commonOptions, monthTicks, monthLabels, grayColor, arc_colors, arc_styles);

% ===== FIGURE S5: BELLEFOUNGOU - DEPTH 2 =====
create_site_depth_figure('Bellefoungou', 2, ...
    XNEE_normal_b, XHoil_2_normal_b, XToil_2_normal_b, ...
    XNEE_extreme_b, XHoil_2_extreme_b, XToil_2_extreme_b, ...
    XNEE_deficient_b, XHoil_2_deficient_b, XToil_2_deficient_b, ...
    xtime_normal_b, ztime_2_normal_b, ytime_2_normal_b, ...
    xtime_extreme_b, ztime_2_extreme_b, ytime_2_extreme_b, ...
    xtime_deficient_b, ztime_2_deficient_b, ytime_2_deficient_b, ...
    commonOptions, monthTicks, monthLabels, grayColor, arc_colors, arc_styles);

%% ========================================================================
%  END OF SCRIPT
%% ========================================================================
% Note: This script assumes all data variables are already loaded in the
% workspace. Run the data preparation script first before executing this.