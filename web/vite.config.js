import { defineConfig } from "vite";

export default defineConfig({
  server: {
    proxy: {
      // For requests to /api/**, pass to backend
      "/api": {
        target: "http://localhost:8080",
      },
    },
  },
});
