flame=FlameGraph/
.PHONY:
test:
	cargo test test_interpreter --color=always -- --nocapture 2>&1
.PHONY:
profile:
	cargo test --release --no-run
	perf record -g `./find_test_executable.sh`
	perf script | $(flame)stackcollapse-perf.pl | $(flame)flamegraph.pl > flame-debug.svg
	chromium-browser flame-debug.svg
