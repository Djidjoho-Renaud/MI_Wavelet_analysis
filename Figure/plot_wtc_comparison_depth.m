function plot_wtc_comparison_depth(data_n, data_b, title_str, ylabel_str, panel_label, depth_label, show_legend)
% PLOT_WTC_COMPARISON_DEPTH Create WTC analysis subplot with depth label
%   plot_wtc_comparison_depth(data_n, data_b, title_str, ylabel_str, panel_label, depth_label, show_legend)
%
%   Creates a subplot with Wavelet Transform Coherence analysis comparing
%   two sites with custom depth labels in the legend
%
%   Inputs:
%       data_n - Normalized data for Nalohou site
%       data_b - Normalized data for Bellefoungou site
%       title_str - Title for the subplot
%       ylabel_str - Label for Y-axis
%       panel_label - Panel identifier (e.g., 'a)', 'b)', etc.)
%       depth_label - Depth label for legend (e.g., '1^{st} depth')
%       show_legend - Boolean, true to display legend
%
%   Example:
%       plot_wtc_comparison_depth(ztime_1_normal_n, ztime_1_normal_b, '', ...
%           '\theta-GP(cm^{6} cm^{-6})', 'd)', '1^{st} depth', false);

    hold on;
    
    % Custom colors for significant arcs
    arc_colors = {'#228C22', '#003314'};  % Green for Nal, Dark green for Bel
    
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
        
        % Modify color of significant arcs (green by default)
        h_arcs = findobj(gca, 'Color', 'g', 'LineWidth', 3);
        if ~isempty(h_arcs)
            set(h_arcs, 'Color', arc_colors{i});
        end
    end
    
    % Restore color order index
    set(gca, 'ColorOrderIndex', original_plot);
    
    % Change black lines (confidence cones) to red
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
    
    % Display legend if requested with depth information
    if show_legend
        legend('', ['Nal:' depth_label], '', ['Bel:' depth_label], ...
               'Location', 'northwest', 'Orientation', 'vertical');
        legend('boxoff');
    end
    
    hold off;
end