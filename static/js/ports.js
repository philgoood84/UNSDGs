let app = Elm.Main.init();



app.ports.updateChartData.subscribe( function (items) {
    google.charts.load('current', {
        'packages':['geochart'],
        // Note: Because this chart requires geocoding, you'll need mapsApiKey.
        // See: https://developers.google.com/chart/interactive/docs/basic_load_libs#load-settings
        //'mapsApiKey': 'AIzaSyD-9tSrke72PouQMnMX-a7eZSW0jkFMBWY'
    });
    google.charts.setOnLoadCallback(drawRegionsMap);
      
    itemsTable = items.map(jsonToGoogleMapObject);
    function drawRegionsMap() {
        var data = new google.visualization.DataTable();
        data.addColumn('string', 'Country');
        data.addColumn('number', 'Score');
        data.addRows(itemsTable)

        var options = {
            title:'BDF Gestion SDG Score',
            sizeAxis: { minValue: -10, maxValue: 10 },
            colorAxis: {colors: ['#fa7e7e', '#fcff8a', '#afed98']},
            backgroundColor: '#ffffff',
            datalessRegionColor: '#cacaca',
            defaultColor: '#f5f5f5',
        };

        var chart = new google.visualization.GeoChart(document.getElementById('chartdiv'));
        chart.draw(data, options);
    };
});

function jsonToGoogleMapObject(item) {
    return [item.id, item.score]
}

/*
    // Create root
    document.getElementById("chartdiv").innerHTML = "";
    var root = am5.Root.new("chartdiv"); 
    // Set themes
    root.setThemes([
        am5themes_Animated.new(root)
    ]);
    // Create chart
    var chart = root.container.children.push(am5map.MapChart.new(root, {
        panX: "rotateX",
        panY: "none",
        projection: am5map.geoNaturalEarth1(),
        layout: root.horizontalLayout
    }));
    // Create polygon series
    var polygonSeries = chart.series.push(am5map.MapPolygonSeries.new(root, {
        geoJSON: am5geodata_worldLow,
        exclude: ["AQ"],
        valueField: "score",
        calculateAggregates: true
    }));

    polygonSeries.mapPolygons.template.setAll({
        tooltipText: "{name}: {score}"
    });

    polygonSeries.set("heatRules", [{
        target: polygonSeries.mapPolygons.template,
        dataField: "value",
        min: am5.color(0xab3433),
        max: am5.color(0x39a459),
        key: "fill"
    }]);

    /*polygonSeries.mapPolygons.template.events.on("pointerover", function(ev) {
        heatLegend.showValue(ev.target.dataItem.get("score"));
    });

    var heatLegend = chart.children.push(am5.HeatLegend.new(root, {
        orientation: "vertical",
        startColor: am5.color(0xab3433),
        endColor: am5.color(0x39a459),
        startText: "Laggards",
        endText: "Leaders",
        stepCount: 1
    }));

    heatLegend.startLabel.setAll({
        fontSize: 12,
        fill: heatLegend.get("startColor")
    });

    heatLegend.endLabel.setAll({
        fontSize: 12,
        fill: heatLegend.get("endColor")
    });

    // change this to template when possible
    polygonSeries.events.on("datavalidated", function () {
        heatLegend.set("startValue", polygonSeries.getPrivate("valueLow"));
        heatLegend.set("endValue", polygonSeries.getPrivate("valueHigh"));
        
    });
    polygonSeries.data.setAll(items);
    */