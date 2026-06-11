#!/usr/bin/env python

from os.path import abspath, dirname
from pathlib import Path
import sys

from e3.testsuite import Testsuite
from e3.testsuite.driver.classic import ClassicTestDriver
from e3.testsuite.result import FailureReason, Log, TestResult, TestStatus


class XMLAdaLegacyDriver(ClassicTestDriver):
    """Legacy driver, it run "make run_test" to execute XMLAda tests."""

    xunit_dir = None

    def run(self):
        root_dir = dirname(dirname(abspath(__file__)))

        self.shell(
            args=["make", "check"],
            cwd=root_dir,
            env=self.env.environ,
            analyze_output=True,
        )

    @staticmethod
    def _read_text(path):
        return path.read_text(encoding="utf-8", errors="replace")

    def analyze(self):
        root_dir = Path(dirname(dirname(abspath(__file__))))
        results_dir = root_dir / "tests" / "results"
        out_files = sorted(results_dir.glob("*.out"))

        if not out_files:
            self.push_error("no legacy output files found in tests/results")
            return

        for out_file in out_files:
            base = out_file.stem
            diff_file = results_dir / (base + ".diff")

            out_content = self._read_text(out_file)
            diff_content = self._read_text(diff_file) if diff_file.exists() else ""
            diff_size = diff_file.stat().st_size if diff_file.exists() else None
            passed = diff_size == 0

            result = TestResult(
                name=f"{self.test_name}.{base}",
                env=self.test_env,
                status=TestStatus.PASS if passed else TestStatus.FAIL,
                msg="" if passed else "diff file is missing or non-empty",
            )

            result.log += out_content

            if not passed:
                result.failure_reasons.add(FailureReason.DIFF)

            # Keep legacy artifacts attached to the e3 result.
            result.out = Log(out_content)
            result.diff = Log(diff_content)

            self.push_result(result)

        return super().analyze()


class XMLAdaTestsuite(Testsuite):
    test_driver_map = {"legacy": XMLAdaLegacyDriver}
    default_driver = "legacy"


if __name__ == "__main__":
    sys.exit(XMLAdaTestsuite().testsuite_main())
