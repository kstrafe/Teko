ta=cat
flame=FlameGraph/
.PHONY:
test:
	(cargo test test_interpreter --color=always -- --nocapture 2>&1 | ${ta} >> pipe) &
.PHONY:
2:
	(cargo build --color=always --features clippy 2>&1 | ${ta} >> pipe) &
.PHONY:
3:
	(cargo test --color=always -- --nocapture 2>&1 | ${ta} >> pipe) &
.PHONY:
profile-debug:
	mkdir -p tmp/
	cargo test --no-run
	perf record -g `./find_test_executable.sh debug`
	perf script | $(flame)stackcollapse-perf.pl | $(flame)flamegraph.pl > tmp/flame-debug.svg
	chromium-browser tmp/flame-debug.svg
.PHONY:
profile-release:
	mkdir -p tmp/
	cargo test --release --no-run
	perf record -g `./find_test_executable.sh release`
	perf script | $(flame)stackcollapse-perf.pl | $(flame)flamegraph.pl > tmp/flame-release.svg
	chromium-browser tmp/flame-release.svg
