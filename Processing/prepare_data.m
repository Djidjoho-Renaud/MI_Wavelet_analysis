% Function to extract and normalize variables from a table
function [XNEE, XToil_1, XToil_2, XHoil_1, XHoil_2, xtime, ytime_1, ytime_2, ztime_1, ztime_2] = ...
    prepare_data(data_table)
    
    % Extract raw data
    XNEE = data_table.NEE;
    XToil_1 = data_table.Tsoil1;
    XToil_2 = data_table.Tsoil2;
    XHoil_1 = data_table.Hsoil1;
    XHoil_2 = data_table.Hsoil2;
    
    % Normalize data
    xtime = normalize_data(XNEE);
    ytime_1 = normalize_data(XToil_1);
    ytime_2 = normalize_data(XToil_2);
    ztime_1 = normalize_data(XHoil_1);
    ztime_2 = normalize_data(XHoil_2);
end