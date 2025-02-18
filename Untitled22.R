//UNCOMMENT WHEN THERE IS A HURRICANE IN THE REGION
var info = L.control({position: 'bottomright'});
info.onAdd = function (map) {
  this._div = L.DomUtil.create('div', 'info'); // create a div with a class "info"
  this._div.innerHTML = '<p id="p1"></p>';
  return this._div;
};

var radarRamp = L.control({position: 'bottomright'});
radarRamp.onAdd = function (map) {
  this._div = L.DomUtil.create('div', 'radarRamp'); // create a div with a class "info"
  this._div.innerHTML = '<img src="https://mesonet.agron.iastate.edu/docs/nexrad_composites/n0q_ramp.png" alt="radar_legend">';
  return this._div;
};


{label: "Probable track of the center", layer: hurricaneTrack},
//{label: "Storm surge probabilities", layer: hurricaneSurge},
//{label: "Storm watches and warnings", layer: hurWatchWMS},
{label: "Surface wind field (knts)", layer: hurricaneWind}

/*
  var stormLegend = L.control({position: 'bottomright'});
  stormLegend.onAdd = function (map) {
    this._div = L.DomUtil.create('div', 'radarRamp'); // create a div with a class "info"
    this._div.innerHTML = '<img src="http://idpgis.ncep.noaa.gov/arcgis/services/NWS_Forecasts_Guidance_Warnings/NHC_Atl_trop_cyclones/MapServer/WmsServer?request=GetLegendGraphic&version=1.3.0&format=image/png&layer=47&" alt="StormSurge_legend">';
    return this._div;
  };
  
  hurricaneSurge.onAdd = function (map) {
    hurSurgeWMS.addTo(map).bringToFront();
    stormLegend.addTo(map);
  };
  
  hurricaneSurge.onRemove = function (map) {
    hurSurgeWMS.remove(map);
    stormLegend.remove(map);
  };
  
  var TWindLegend = L.control({position: 'bottomright'});
  TWindLegend.onAdd = function (map) {
    this._div = L.DomUtil.create('div', 'radarRamp'); // create a div with a class "info"
    this._div.innerHTML = '<img src="https://idpgis.ncep.noaa.gov/arcgis/services/NWS_Forecasts_Guidance_Warnings/NHC_Atl_trop_cyclones/MapServer/WmsServer?request=GetLegendGraphic&version=1.3.0&format=image/png&layer=49&" alt="WindField_legend">';
    return this._div;
  };
  
  hurricaneWind.onAdd = function (map) {
    hurWindWMS.addTo(map).bringToFront();
    TWindLegend.addTo(map);
  };
  
  hurricaneWind.onRemove = function (map) {
    hurWindWMS.remove(map);
    TWindLegend.remove(map);
  };
  */