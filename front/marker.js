import L from 'leaflet';

const icon_types = {
  "blue": "blue.png",
  "red": "red.png",
  "green": "green.png",
  "purple": "purple.png",
  "loc_blue": "loc_blue.png",
  "loc_red": "loc_red.png",
  "loc_yellow": "loc_yellow.png"
};

function makeMarker(coord=[0, 0], icon='loc_blue', text='') {
  let markerIcon = L.icon({
    iconSize:     [30, 30],
    iconAnchor:   [15, 15],
    iconUrl: './marker/' + icon_types[icon]
  });

  let [x, y] = coord;
  y = y/8 - (15360/8);
  x = x/8;
  let marker = L.marker(L.latLng(y, x), {icon: markerIcon});
  // https://gis.stackexchange.com/questions/59571/how-to-add-text-only-labels-on-leaflet-map-with-no-icon
  marker.bindTooltip(text, {permanent: true, offset: [15, 0]});
  return marker;
}

function makeUnitPos(coord=[0, 0], icon='purple') {
  let markerIcon = L.icon({
    iconSize:     [16, 16],
    iconAnchor:   [8, 8],
    iconUrl: './marker/' + icon_types[icon]
  });

  let [x, y] = coord;
  y = y/8 - (15360/8);
  x = x/8;
  let marker = L.marker(L.latLng(y, x), {icon: markerIcon});
  return marker;
}

export {icon_types, makeMarker, makeUnitPos};

