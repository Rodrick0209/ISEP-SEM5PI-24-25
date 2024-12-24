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
    target: "http://10.9.22.72:2226",
    secure: false,
    logLevel: "debug",
    changeOrigin: true
  }
];

module.exports = PROXY_CONFIG;
