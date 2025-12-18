%% ========================================================================
%  FUNCTION: Create Global Coherence subplot
%% ========================================================================
function create_coherence_subplot(data_nee, data_moisture, data_temp, subplot_title, ...
                                   xlabel_text, show_legend, legend_moisture, legend_temp, ...
                                   grayColor, arc_colors, arc_styles)
    hold on;
    
    % Save color order index
    original_plot = get(gca, 'ColorOrderIndex');
    
    % ===== FIRST ANALYSIS: NEE - Moisture =====
    set(gca, 'ColorOrderIndex', 1);
    rng(1589);
    arcwisetest_globalcoher(data_nee, data_moisture, 'Montepw', 3000, 'Narc', 1000, ...
                           'Dir', 'Horiz', 'MaxScale', 128);
    
    % Modify significant arcs for moisture
    h_arcs_moisture = findobj(gca, 'Color', 'g', 'LineWidth', 3);
    if ~isempty(h_arcs_moisture)
        set(h_arcs_moisture(1), 'Color', arc_colors{1}, 'LineStyle', arc_styles{1});
    end
    
    % ===== SECOND ANALYSIS: NEE - Temperature =====
    set(gca, 'ColorOrderIndex', 2);
    rng(1589);
    arcwisetest_globalcoher(data_nee, data_temp, 'Montepw', 3000, 'Narc', 1000, ...
                           'Dir', 'Horiz', 'MaxScale', 128);
    
    % Modify significant arcs for temperature
    h_arcs_temp = findobj(gca, 'Color', 'g', 'LineWidth', 3);
    if ~isempty(h_arcs_temp)
        set(h_arcs_temp(1), 'Color', arc_colors{2}, 'LineStyle', arc_styles{2});
    end
    
    % Restore color order index
    set(gca, 'ColorOrderIndex', original_plot);
    
    % Modify black lines (confidence bounds)
    lines = findobj(gca, 'Type', 'line', 'Color', 'k');
    if length(lines) >= 2
        set(lines(1), 'Color', grayColor, 'LineStyle', '--', 'LineWidth', 2);
        set(lines(2), 'Color', grayColor, 'LineStyle', '-', 'LineWidth', 2);
    end
    
    % Configure axes and labels
    if isempty(xlabel_text)
        xlabel(' ');
    else
        xlabel(xlabel_text, 'FontSize', 9);
    end
    ylabel(' ');
    title(subplot_title, 'FontSize', 10);
    
    % Add legend if requested
    if show_legend
        legend('', legend_moisture, '', legend_temp, ...
               'Orientation', 'vertical', 'Location', 'northeast', 'FontSize', 8);
        set(gca, 'XLim', [0, 1]);
        legend('boxoff');
    end
    
    hold off;
end
