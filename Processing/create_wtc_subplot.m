%% ========================================================================
%  FUNCTION: Create WTC subplot with standard formatting
%% ========================================================================
function create_wtc_subplot(data1, data2, subplot_title, ylabel_text, xlabel_text, commonOpts, monthTicks, monthLabels)
    wtc(data1, data2, commonOpts{:});
    set(gca, 'XTick', monthTicks, 'XTickLabel', monthLabels, 'layer', 'top');
    title(subplot_title, 'FontSize', 10);
    colormap(jet);
    ylabel(ylabel_text, 'FontSize', 9);
    if ~isempty(xlabel_text)
        xlabel(xlabel_text, 'FontSize', 9);
    end
end