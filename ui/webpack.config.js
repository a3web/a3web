const path = require('path');

module.exports = {
  entry: './index.js',
  output: {
    filename: 'index.js',
    path: path.resolve(__dirname, '../static/'),
  },
  module: {
    rules: [
      { test: /\.css$/, loader: 'style-loader!css-loader' },
      {
        test: /\.png$/,
        loader: 'url-loader',
        query: { mimetype: 'image/png' }
      }
    ]
  }
};
