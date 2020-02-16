import L from 'leaflet';
import 'leaflet/dist/leaflet.css';
import * as marker from './marker.js';

let map = L.map('map', {
  crs: L.CRS.Simple,
  minZoom: -4
});

let bounds = [[0,0], [15360, 15360]];
let image = L.imageOverlay('./maps/Tanoa.png', bounds).addTo(map);
map.fitBounds(bounds);

let UNITS_POS_MARKER = [];

function update_units_pos () {
  let sideToType = {
    "WEST": "blue",
    "EAST": "red",
    "GUER": "green",
    "CIV": "purple"
  };

  fetch('/units-pos')
    .then((response) => {
      return response.json();
    })
    .then((units_pos_array) => {
      for(let marker of UNITS_POS_MARKER) {
	map.removeLayer(marker);
      }
      UNITS_POS_MARKER = [];

      for(let u of units_pos_array) {
	let pos_marker = marker.makeUnitPos(
	  u.position,
	  sideToType[u.side]
	);
	UNITS_POS_MARKER.push(pos_marker);
	pos_marker.addTo(map);
      }
    });
}

setInterval(update_units_pos, 1000);

