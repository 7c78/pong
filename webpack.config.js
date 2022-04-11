let path = require("path")
let HtmlWebpackPlugin = require("html-webpack-plugin")
let CopyPlugin = require("copy-webpack-plugin")

module.exports = {
    mode: "development",
    entry: "./src/App.fsproj",
    devtool: "inline-source-map",
    output: {
        path: path.join(__dirname, "./build"),
        filename: "app.js"
    },
    devServer: {
        static: {
            directory: "./build"
        },
        port: 8080
    },
    module: {
        rules: [
            {
                test: /\.fs(x|proj)?$/,
                use: "fable-loader"
            }
        ]
    },
    plugins: [
        new CopyPlugin({
            patterns: [
                { from: "src/app.css", to: "app.css" }
            ]
        }),

        new HtmlWebpackPlugin({
            filename: "./index.html",
            template: "./src/index.html"
        })

    ]
}
