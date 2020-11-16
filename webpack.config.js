var path = require('path');

module.exports = {
    mode: 'development',
    entry: {
        single: path.join(__dirname, 'srcjs', 'customSlider.jsx'),
        multi: path.join(__dirname, 'srcjs', 'customSliderTriple.jsx')
    },
    output: {
        path: path.join(__dirname, 'inst', 'www', '${package}', 'customSliders'),
        path: path.join(__dirname, 'inst/www/customSlider/customSliders'),
        filename: '[name].js'
    },
    module: {
        rules: [
            {
                test: /\.jsx?$/,
                loader: 'babel-loader',
                options: {
                    presets: ['@babel/preset-env', '@babel/preset-react']
                }
            }
        ]
    },
    externals: {
        'react': 'window.React',
        'react-dom': 'window.ReactDOM',
        'reactR': 'window.reactR'
    },
    stats: {
        colors: true
    },
    devtool: 'source-map'
};
