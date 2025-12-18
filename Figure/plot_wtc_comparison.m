function plot_wtc_comparison(data_n, data_b, title_str, ylabel_str, panel_label, show_legend)
% PLOT_WTC_COMPARISON Create WTC analysis subplot for two sites

    hold on;
    
    % Custom colors for significant arcs
    arc_colors = {'#228C22', '#003314'};
    
    % Save color order index
    original_plot = get(gca, 'ColorOrderIndex');
    
    % Plot for Nalohou (i=1) and Bellefoungou (i=2)
    for i = 1:2
        set(gca, 'ColorOrderIndex', i);
        
        % WTC analysis with global power
        if i == 1
            arcwisetest_globalpower(data_n, 'MakeFigure', '0', 'MaxScale', 800);
        else
            arcwisetest_globalpower(data_b, 'MakeFigure', '0', 'MaxScale', 800);
        end
        
        % Modify color of significant arcs
        h_arcs = findobj(gca, 'Color', 'g', 'LineWidth', 3);
        if ~isempty(h_arcs)
            set(h_arcs, 'Color', arc_colors{i});
        end
    end
    
    % Restore color order index
    set(gca, 'ColorOrderIndex', original_plot);
    
    % Change black lines to red
    lines = findobj(gca, 'Type', 'line', 'Color', 'k');
    if length(lines) >= 2
        set(lines(1), 'Color', 'r', 'LineStyle', '-');
        set(lines(2), 'Color', 'r', 'LineStyle', '-');
    end
    
    % Configure axes and labels
    set(gca, 'YLim', [0, 250]);
    ylabel(ylabel_str);
    title(title_str);
    text(0.05, 0.95, panel_label, 'Units', 'normalized', 'FontWeight', 'bold');
    
    % Display legend if requested
    if show_legend
        legend('', 'Nal', '', 'Bel', 'Orientation', 'vertical', 'Location', 'northwest');
        legend('boxoff');
    end
    
    hold off;
end