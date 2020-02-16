import L from 'leaflet';
import 'leaflet/dist/leaflet.css';

var map = L.map('map', {
    crs: L.CRS.Simple,
    minZoom: -4
});

var bounds = [[0,0], [15360, 15360]];
var image = L.imageOverlay('./maps/Tanoa.png', bounds).addTo(map);

map.fitBounds(bounds);
