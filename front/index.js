import L from 'leaflet';
import 'leaflet/dist/leaflet.css';

let map = L.map('map', {
    crs: L.CRS.Simple,
    minZoom: -4
});

let bounds = [[0,0], [15360, 15360]];
let image = L.imageOverlay('./maps/Tanoa.png', bounds).addTo(map);
map.fitBounds(bounds);

function makeIcon() {
  let markerIcon = L.icon({
      iconSize:     [30, 30],
      iconAnchor:   [15, 15],
      iconUrl: './marker/dot.png'
  });
  return markerIcon;
}

function makeMarker(coord, text) {
  let [x, y] = coord;
  let marker = L.marker(L.latLng(y, x), {icon: makeIcon(text)});
  // https://gis.stackexchange.com/questions/59571/how-to-add-text-only-labels-on-leaflet-map-with-no-icon
  marker.bindTooltip(text, {permanent: true, offset: [15, 0]});
  return marker;
}

let marker1 = makeMarker([0, 0], "测试aaaaaaaaaaaaaaaaa bbbbbbbbbb cccc");
marker1.addTo(map);

