#!/usr/bin/python3
from os import listdir
from os.path import isfile, join
import subprocess

testdir = "tests"
testfiles = [f for f in listdir(testdir) if isfile(join(testdir, f)) and f.endswith(".iit")]

print(f"Found {len(testfiles)} test files")

num_failed = 0
num_passed = 0
failed_files = []

try:
    for testfile in testfiles:
        with open(join(testdir, testfile)) as f:
            if testfile.startswith("synerr"):
                expected_result = ""
                expected_exit_code = 1
            elif testfile.startswith("semerr"):
                expected_result = ""
                expected_exit_code = 2
            else:
                lines = f.readlines()
                expected_result = lines[0].split(":")[1].strip("\n").strip(" ")
                expected_exit_code = 0
            process = subprocess.run(["./iitran", join(testdir, testfile)], capture_output=True, text=True)
            exit_code = process.returncode
            result = process.stdout.strip("\n").strip(" ")
            if result != expected_result or expected_exit_code != exit_code:
                print(f"{testfile}: expected result to be '{expected_result}' with exit code {expected_exit_code}, got '{result}' with exit code {exit_code}")
                num_failed += 1
                failed_files.append(testfile)
            else:
                print(f"{testfile}: passed")
                num_passed += 1
except Exception as err:
    print(f"Error while running tests: {err}")
finally:
    print(f"Result: {num_passed} passed, {num_failed} failed, {len(testfiles) - num_passed - num_failed} ignored")
    if failed_files:
        print(f"Failed test: {', '.join(failed_files)}")
