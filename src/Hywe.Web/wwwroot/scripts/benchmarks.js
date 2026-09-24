// HYWE WebAssembly Benchmark & Conformance Test Suite Runner

window.runAndDownloadBenchmark = async function (methodName, filename) {
    console.log("%c[TEST RUNNER] Starting " + methodName + "... (The browser UI will yield while computing)", "color: #f39c12; font-weight: bold;");
    try {
        let clientDetails = navigator.userAgent || "Unknown Browser";
        if (navigator.hardwareConcurrency) {
            clientDetails += " (" + navigator.hardwareConcurrency + " logical threads";
            if (navigator.deviceMemory) clientDetails += ", ~" + navigator.deviceMemory + " GB RAM";
            clientDetails += ")";
        }
        const result = await DotNet.invokeMethodAsync('Hywe.Web', methodName, clientDetails);
        const blob = new Blob([result], { type: 'text/markdown' });
        const url = window.URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.style.display = 'none';
        a.href = url;
        a.download = filename;
        document.body.appendChild(a);
        a.click();
        window.URL.revokeObjectURL(url);
        document.body.removeChild(a);
        console.log("%c[TEST RUNNER] Successfully completed and downloaded " + filename + "!", "color: #2ecc71; font-weight: bold;");
        return result;
    } catch (err) {
        console.error("[TEST RUNNER] Benchmark failed:", err);
        throw err;
    }
};

window.runPerformanceTest = () => window.runAndDownloadBenchmark('RunPerformanceBenchmark', 'PerformanceBenchmark.md');
window.runConformanceTest = () => window.runAndDownloadBenchmark('RunConformanceTests', 'ConformanceTests.md');
window.runQualityTest = () => window.runAndDownloadBenchmark('RunQualityBenchmarks', 'QualityBenchmarks.md');
window.runScalingTest = () => window.runAndDownloadBenchmark('RunScalingBenchmarks', 'ScalingBenchmarks.md');
window.runEndToEndTest = () => window.runAndDownloadBenchmark('RunEndToEndLatencyProfile', 'EndToEndLatency.md');
window.runMultiContainerTest = () => window.runAndDownloadBenchmark('RunMultiContainerScaling', 'MultiContainerScaling.md');
window.runSensitivityTest = () => window.runAndDownloadBenchmark('RunPerturbationSensitivity', 'PerturbationSensitivity.md');

window.runAllTests = async function () {
    console.log("%c[TEST RUNNER] Initiating Full Benchmark Suite...", "color: #3498db; font-weight: bold;");
    console.log("%c-> [1/7] Running Quality Benchmarks...", "color: #f39c12;");
    const q = await window.runQualityTest();
    console.log("%c-> [2/7] Running Conformance & Repeatability Tests...", "color: #f39c12;");
    const c = await window.runConformanceTest();
    console.log("%c-> [3/7] Running Performance Benchmark...", "color: #f39c12;");
    const p = await window.runPerformanceTest();
    console.log("%c-> [4/7] Running Scaling Benchmark (10 to 1,000 nodes)...", "color: #f39c12;");
    const s = await window.runScalingTest();
    console.log("%c-> [5/7] Running End-to-End Latency Benchmark...", "color: #f39c12;");
    const e2e = await window.runEndToEndTest();
    console.log("%c-> [6/7] Running Multi-Container Scaling Benchmark...", "color: #f39c12;");
    const mc = await window.runMultiContainerTest();
    console.log("%c-> [7/7] Running Perturbation Sensitivity Benchmark...", "color: #f39c12;");
    const sens = await window.runSensitivityTest();
    console.log("%c[TEST RUNNER] All benchmark suites completed successfully! Check your downloads folder.", "color: #2ecc71; font-size: 14px; font-weight: bold;");
    return { quality: q, conformance: c, performance: p, scaling: s, endToEnd: e2e, multiContainer: mc, sensitivity: sens };
};

console.log(
    "%c🧪 HYWE WebAssembly Benchmark & Conformance Test Suite",
    "background: #1e1e2e; color: #89b4fa; font-size: 14px; font-weight: bold; padding: 6px 12px; border-radius: 4px; border-left: 4px solid #89b4fa; margin-top: 8px;"
);
console.log(
    "%cRun empirical WebAssembly tests directly from DevTools and auto-download Markdown reports:\n\n" +
    "%c1. runQualityTest()          %c— 6 Quantitative Topological Descriptors (D, CI, B, P, ΔJ, σ_A) (~1-2 min)\n" +
    "%c2. runConformanceTest()      %c— Determinism & signature hash repeatability across 10 iterations (~2 min)\n" +
    "%c3. runPerformanceTest()      %c— Cold vs. warm latency & standard deviation across 24 operators (~3-4 min)\n" +
    "%c4. runScalingTest()          %c— Algorithmic scaling latency from 10 to 1,000 nodes (~3-5 min)\n" +
    "%c5. runEndToEndTest()         %c— End-to-End latency profiling (AST → SVG → DOM → WebGPU) (~1-2 min)\n" +
    "%c6. runMultiContainerTest()   %c— Combinatorial multi-container scaling across floorplates (~2-3 min)\n" +
    "%c7. runSensitivityTest()      %c— Syntactic perturbation sensitivity (D(C_0, C_1)) (~1 min)\n" +
    "%c8. runAllTests()             %c— Execute all 7 suites sequentially with automated downloads\n\n" +
    "%cAdvanced: %crunAndDownloadBenchmark('RunQualityBenchmarks', 'CustomQualityReport.md')",
    "color: #a6adc8; font-size: 12px;",
    "color: #a6e3a1; font-weight: bold; font-family: monospace;", "color: #cdd6f4;",
    "color: #a6e3a1; font-weight: bold; font-family: monospace;", "color: #cdd6f4;",
    "color: #a6e3a1; font-weight: bold; font-family: monospace;", "color: #cdd6f4;",
    "color: #a6e3a1; font-weight: bold; font-family: monospace;", "color: #cdd6f4;",
    "color: #a6e3a1; font-weight: bold; font-family: monospace;", "color: #cdd6f4;",
    "color: #a6e3a1; font-weight: bold; font-family: monospace;", "color: #cdd6f4;",
    "color: #a6e3a1; font-weight: bold; font-family: monospace;", "color: #cdd6f4;",
    "color: #f9e2af; font-weight: bold; font-family: monospace;", "color: #cdd6f4;",
    "color: #fab387; font-weight: bold;", "color: #89dceb; font-family: monospace;"
);
