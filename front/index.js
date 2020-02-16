import 'ol/ol.css';
import Map from 'ol/Map';
import View from 'ol/View';
import {getCenter} from 'ol/extent';
import ImageLayer from 'ol/layer/Image';
import Projection from 'ol/proj/Projection';
import Static from 'ol/source/ImageStatic';

import {Vector as VectorLayer} from 'ol/layer';
import VectorSource from 'ol/source/Vector';

import Feature from 'ol/Feature';
import Point from 'ol/geom/Point';

var testMarker = new Feature({
  type: 'icon',
  geometry: new Point([7000.0, 9000.0])
});

var vectorLayer = new VectorLayer({
  source: new VectorSource({
    features: [testMarker]
  })
})

var extent = [0, 0, 15360, 15359];
var projection = new Projection({
  code: 'xkcd-image',
  units: 'pixels',
  extent: extent
});

var map = new Map({
  layers: [
    new ImageLayer({
      source: new Static({
        attributions: '© <a href="http://xkcd.com/license.html">xkcd</a>',
        url: './maps/Tanoa.png',
        projection: projection,
        imageExtent: extent
      })
    }),
    vectorLayer
  ],
  target: 'map',
  view: new View({
    projection: projection,
    center: getCenter(extent),
    zoom: 2,
    maxZoom: 8
  })
});

// icon and text
