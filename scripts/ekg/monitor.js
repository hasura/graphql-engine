$(document).ready(function () {
    "use strict";

    var hge_port;
    function set_hge_port(){
        // Maybe use query strings if we need to do more configuration:
        if(window.location.hash) {
            hge_port = window.location.hash.substring(1);
        } else {
            alert("Please provide the graphql-engine port in the URL fragment, e.g.: .../ekg.html#8088");
        }
    }
    window.onhashchange = set_hge_port;
    set_hge_port();

    // Number formatters
    function commaify(n)
    {
        var nStr = n.toString();
        var x = nStr.split('.');
        var x1 = x[0];
        var x2 = x.length > 1 ? '.' + x[1] : '';
        var rgx = /(\d+)(\d{3})/;
        while (rgx.test(x1)) {
            x1 = x1.replace(rgx, '$1' + ',' + '$2');
        }
        return x1 + x2;
    }

    function formatSuffix(val, opt_prec) {
        if (val === null) {
            return "N/A";
        }

        var prec = opt_prec || 1;
        if (val >= 1000000000) {
            return (val / 1000000000).toFixed(prec) + " GB";
        } else if (val >= 1000000) {
            return (val / 1000000).toFixed(prec) + " MB";
        } else if (val >= 1000) {
            return (val / 1000).toFixed(prec) + " kB";
        } else {
            return val.toFixed(prec) + " B";
        }
    }

    function formatRate(val, prec) {
        if (val === null) {
            return "N/A";
        }

        return formatSuffix(val, prec) + "/s";
    }

    function formatPercent(val, opt_prec) {
        if (val === null) {
            return "N/A";
        }

        var prec = opt_prec || 1;
        return val.toFixed(prec) + " %";
    }

    // Set up polling interval control
    var updateInterval = 1000;  // ms
    $("#updateInterval").val(updateInterval).change(function () {
        updateInterval = $(this).val();
    });

    // Allow the UI to be paused
    var paused = false;
    $('#pause-ui').click(function () {
        if (paused) {
            $(this).text("Pause UI");
            paused = false;
        } else {
            $(this).text("Unpause UI");
            paused = true;
        }
    });

    // Plot formatters
    function suffixFormatter(val, axis) {
        return formatSuffix(val, axis.tickDecimals);
    }

    function suffixFormatterGeneric(val, axis) {
        if (val >= 1000000000) {
            return (val / 1000000000).toFixed(axis.tickDecimals) + " G";
        } else if (val >= 1000000) {
            return (val / 1000000).toFixed(axis.tickDecimals) + " M";
        } else if (val >= 1000) {
            return (val / 1000).toFixed(axis.tickDecimals) + " k";
        } else {
            return val.toFixed(axis.tickDecimals);
        }
    }

    function rateFormatter(val, axis) {
        return formatRate(val, axis.tickDecimals);
    }

    function percentFormatter(val, axis) {
        return formatPercent(val, axis.tickDecimals);
    }

    // Fetch data periodically and notify interested parties.
    var listeners = [];

    function subscribe(fn) {
        listeners.push(fn);
    }

    function unsubscribe(fn) {
        listeners = listeners.filter(function (el) {
            if (el !== fn) {
                return el;
            }
        });
    }

    var alertVisible = false;
    function fetchData() {
        function onDataReceived(stats) {
            if (alertVisible) {
                $(".alert-message").hide();
            }
            alertVisible = false;
            for (var i = 0; i < listeners.length; i++) {
                listeners[i](stats, stats.ekg.server_timestamp_ms[0].value.val);
            }
        }

        function onError() {
            $(".alert-message").show();
            alertVisible = true;
        }

        $.ajax("http://127.0.0.1:"+hge_port+"/dev/ekg", {
            dataType: 'json',
            success: onDataReceived,
            error: onError,
            cache: false
        });

        setTimeout(fetchData, updateInterval);
    }
    fetchData();

    function addPlot(elem, series, opts) {
        var defaultOptions = {
            series: { shadowSize: 0 },  // drawing is faster without shadows
            xaxis: { mode: "time", tickSize: [10, "second"] }
        };
        var options = $.extend(true, {}, defaultOptions, opts);
        var data = new Array(series.length);
        var maxPoints = 60;
        for(var i = 0; i < series.length; i++) {
            data[i] = [];
        }

        var plot = $.plot(elem, [], options);

        var prev_stats, prev_time;
        function onDataReceived(stats, time) {
            for(var i = 0; i < series.length; i++) {
                if (data[i].length >= maxPoints) {
                    data[i] = data[i].slice(1);
                }

                data[i].push([time, series[i].fn(stats, time,
                                                 prev_stats, prev_time)]);

                // the data may arrive out-of-order, so sort by time stamp first
                data[i].sort(function (a, b) { return a[0] - b[0]; });
            }

            // zip legends with data
            var res = [];
            for(var i = 0; i < series.length; i++)
                res.push({ label: series[i].label, data: data[i] });

            if (!paused) {
                plot.setData(res);
                plot.setupGrid();
                plot.draw();
            }
            prev_stats = stats;
            prev_time = time;
        }

        subscribe(onDataReceived);
        return onDataReceived;
    }

    function addCounter(elem, fn, formatter) {
        var prev_stats, prev_time;
        function onDataReceived(stats, time) {
            if (!paused)
                elem.text(formatter(fn(stats, time, prev_stats, prev_time)));
            prev_stats = stats;
            prev_time = time;
        }

        subscribe(onDataReceived);
    }

    // jQuery has problem with IDs containing symbols, so we replace the
    // symbols generated by `JSON.stringify`.
    function safeStringifyTags(tags) {
        return JSON.stringify(tags)
                .replace(/\{|\}|\"/g, '')
                .replace(/:/g, "-COLON-")
                .replace(/,/g, "-COMMA-")
    }

    function addDynamicPlot(name, tags, button, graph_fn, label_fn) {
        function getStats(stats, time, prev_stats, prev_time) {
            return graph_fn(name, stats, time, prev_stats, prev_time);
        }

        // jQuery has problem with IDs containing dots and other symbols, so we
        // need to strip symbols out.
        //
        // Here we are also relying on `JSON.stringify` to produce strings from
        // objects in a consistent manner. This might break.
        var tagsStr = safeStringifyTags(tags);
        var plotId = name.replace(/\./g, '-') + '-TAGS-' + tagsStr + '-PLOT';
        $("#plots:last").append(
            '<div id="' + plotId + '" class="plot-container">' +
                '<img src="cross.png" class="close-button"><h3>' +
                name + JSON.stringify(tags) +
                '</h3><div class="plot"></div></div>');
        var plot = $("#plots > .plot-container:last > div");
        var observer = addPlot(plot,
                [{ label: label_fn(name), fn: getStats }],
                { yaxis: { tickFormatter: suffixFormatterGeneric } });

        var plotContainer = $("#" + plotId);
        var closeButton = plotContainer.find("img");
        closeButton.hide();
        closeButton.click(function () {
            plotContainer.remove();
            button.show();
            unsubscribe(observer);
        });

        plotContainer.hover(
            function () {
                closeButton.show();
            },
            function () {
                closeButton.hide();
            }
        );
    }

    function addMetrics(table) {
        var COUNTER = "c";
        var GAUGE = "g";
        var DISTRIBUTION = "d";
        var metrics = {};

        function getStatValue(stats, pieces, tags) {
            var value = stats;
            $.each(pieces, function(_, piece) {
                value = value[piece];
            });
            // The value should now be an array of objects of the form
            // { "tags": tagsObj, "value": valueObj }
            var returnVal;
            var tagsStr = JSON.stringify(tags);
            // Due to this loop, processing received data takes O(n^2) time,
            // where n is the size of the largest group of metrics sharing the
            // same name. We will eventually need to fix this.
            $.each(value, function(_, tagValueObj) {
              // Here we are relying on `JSON.stringify` to produce strings
              // from objects in a consistent manner. This might break.
                if (tagsStr === JSON.stringify(tagValueObj.tags)) {
                    returnVal = tagValueObj.value;
                }
            });
            return returnVal;
        }

        function makeDataGetter(name, tags) {
            var pieces = name.split(".");
            function get(name, stats, time, prev_stats, prev_time) {
                var value = getStatValue(stats, pieces, tags);
                if (value.type === COUNTER) {
                    if (prev_stats == undefined)
                        return null;
                    var prev_value = getStatValue(prev_stats, pieces, tags);
                    return 1000 * (value.val - prev_value.val) /
                        (time - prev_time);
                } else if (value.type === DISTRIBUTION) {
                    return value.mean;
                } else {  // value.type === GAUGE || value.type === LABEL
                    return value.val;
                }
            }
            return get;
        }

        function counterLabel(label) {
            return label + "/s";
        }

        function gaugeLabel(label) {
            return label;
        }

        /** Adds the table row. */
        function addElem(name, tags, value) {
            var name_with_tags = name+'-TAGS-'+safeStringifyTags(tags);
            var elem;
            if (name_with_tags in metrics) {
                elem = metrics[name_with_tags];
            } else {
                // Add UI element
                table.find("tbody:last").append(
                    '<tr><td>' + name + '<br>' +
                        JSON.stringify(tags) + '</td>' +
                      '<td>' +
                        ' <img src="chart_line_add.png" class="graph-button"' +
                        ' width="16" height="16"' +
                        ' alt="Add graph" title="Add graph"></td>' +
                      '<td class="value">N/A</td></tr>');
                elem = table.find("tbody > tr > td:last");
                metrics[name_with_tags] = elem;

                var button = table.find("tbody > tr:last > td:eq(1) > img");
                var graph_fn = makeDataGetter(name, tags);
                var label_fn = gaugeLabel;
                if (value.type === COUNTER) {
                    label_fn = counterLabel;
                }
                button.click(function () {
                    addDynamicPlot(name, tags, button, graph_fn, label_fn);
                    $(this).hide();
                });
            }
            if (!paused) {
                if (value.type === DISTRIBUTION) {
                    if (value.mean !== null) {
                        var val = value.mean.toPrecision(8) + '\n+/-' +
                            Math.sqrt(value.variance).toPrecision(8) + ' sd';
                    }
                    else {
                        var val = "N/A";
                    }
                } else {  // COUNTER, GAUGE, LABEL
                    var val = value.val;
                }
                if ($.inArray(value.type, [COUNTER, GAUGE]) !== -1) {
                    val = commaify(val);
                }
                elem.text(val);
            }
        }

        /** Updates UI for all metrics. */
        function onDataReceived(stats, time) {
            function build(prefix, obj) {
                $.each(obj, function (suffix, value) {
                    if (Array.isArray(value)) {
                        $.each(value, function (_, arr_value) {
                            var name = prefix + suffix;
                            addElem(name, arr_value.tags, arr_value.value);
                        });
                    } else {
                        build(prefix + suffix + '.', value);
                    }
                });
            }
            build('', stats);
        }

        subscribe(onDataReceived);
    }

    function initAll() {
        // Metrics
        var current_bytes_used = function (stats) {
            return stats.rts.gc.current_bytes_used[0].value.val;
        };
        var max_bytes_used = function (stats) {
            return stats.rts.gc.max_bytes_used[0].value.val;
        };
        var max_bytes_slop = function (stats) {
            return stats.rts.gc.max_bytes_slop[0].value.val;
        };
        var current_bytes_slop = function (stats) {
            return stats.rts.gc.current_bytes_slop[0].value.val;
        };
        var productivity_wall_percent = function (stats, time, prev_stats, prev_time) {
            if (prev_stats == undefined)
                return null;
            var mutator_ms = stats.rts.gc.mutator_wall_ms[0].value.val -
                prev_stats.rts.gc.mutator_wall_ms[0].value.val;
            var gc_ms = stats.rts.gc.gc_wall_ms[0].value.val -
                prev_stats.rts.gc.gc_wall_ms[0].value.val;
            return 100 * mutator_ms / (mutator_ms + gc_ms);
        };
        var productivity_cpu_percent = function (stats, time, prev_stats, prev_time) {
            if (prev_stats == undefined)
                return null;
            var mutator_ms = stats.rts.gc.mutator_cpu_ms[0].value.val -
                prev_stats.rts.gc.mutator_cpu_ms[0].value.val;
            var gc_ms = stats.rts.gc.gc_cpu_ms[0].value.val -
                prev_stats.rts.gc.gc_cpu_ms[0].value.val;
            return 100 * mutator_ms / (mutator_ms + gc_ms);
        };
        var allocation_rate = function (stats, time, prev_stats, prev_time) {
            if (prev_stats == undefined)
                return null;
            return 1000 * (stats.rts.gc.bytes_allocated[0].value.val -
                           prev_stats.rts.gc.bytes_allocated[0].value.val) /
                (time - prev_time);
        };

        addMetrics($("#metric-table"));

        // Plots
        addPlot($("#current-bytes-used-plot > div"),
                [{ label: "residency", fn: current_bytes_used }],
                { yaxis: { tickFormatter: suffixFormatter } });
        addPlot($("#allocation-rate-plot > div"),
                [{ label: "rate", fn: allocation_rate }],
                { yaxis: { tickFormatter: rateFormatter } });
        addPlot($("#productivity-plot > div"),
                [{ label: "wall clock time", fn: productivity_wall_percent },
                 { label: "cpu time", fn: productivity_cpu_percent }],
                { yaxis: { tickDecimals: 1, tickFormatter: percentFormatter } });

        // GC and memory statistics
        addCounter($("#max-bytes-used"), max_bytes_used, formatSuffix);
        addCounter($("#current-bytes-used"), current_bytes_used, formatSuffix);
        addCounter($("#max-bytes-slop"), max_bytes_slop, formatSuffix);
        addCounter($("#current-bytes-slop"), current_bytes_slop, formatSuffix);
        addCounter($("#productivity-wall"), productivity_wall_percent, formatPercent);
        addCounter($("#productivity-cpu"), productivity_cpu_percent, formatPercent);
        addCounter($("#allocation-rate"), allocation_rate, formatRate);
    }

    initAll();
});
