const path = require('path');
const webpack = require('webpack');

module.exports = {
    entry: './raw_stubs.js',
    mode: 'production',
    target: 'web',
    output: {
        path: path.resolve(__dirname, '.'),
        filename: 'stubs.js'
    },
    resolve: {
        fallback: {
            "buffer": require.resolve("buffer"),
            "crypto": require.resolve("crypto-browserify"),
            "stream": require.resolve("stream-browserify"),
            "assert": require.resolve("assert"),
            "http": require.resolve("stream-http"),
            "https": require.resolve("https-browserify"),
            "os": require.resolve("os-browserify"),
            "url": require.resolve("url"),
            "path": require.resolve("path-browserify"),
            "fs": false
        }
    },
    plugins: [
        new webpack.ProvidePlugin({ Buffer: ['buffer', 'Buffer'] }),
        new webpack.ProvidePlugin({ Buffer: 'process/browser' }),
    ]
}
