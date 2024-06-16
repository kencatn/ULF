import { defineConfig } from 'vite'
// https://vitejs.dev/config/
export default defineConfig({
    server: {
        watch: {
            usePolling: true,
            ignored: [
                "**/*.fs" // Don't watch F# files
            ]
        }
    },
    root: "./src/Fable",
    build: {
        outDir: "../../docs",
        emptyOutDir: true,
        sourcemap: true
    },
    base: "./"
})