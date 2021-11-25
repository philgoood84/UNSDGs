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
