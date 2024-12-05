const PROXY_CONFIG = [
  {
    context: ["/api2"],
    target: "http://localhost:4000",
    secure: false,
    logLevel: "debug",
    changeOrigin: true
  },
  {
    context: ["/api"],
    target: "https://localhost:5001",
    secure: false,
    logLevel: "debug",
    changeOrigin: true
  }
];

module.exports = PROXY_CONFIG;
