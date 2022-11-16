/** @type {import('next').NextConfig} */
const nextConfig = {
  experimental: {
    appDir: true,
    runtime: "experimental-edge",
  },
  images: {
    remotePatterns: [
      {
        protocol: "http",
        hostname: "img6a.flixcart.com",
      },
      {
        protocol: "http",
        hostname: "img5a.flixcart.com",
      },
    ],
  },
};

module.exports = nextConfig;
