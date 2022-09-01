#!/usr/bin/python3
from os import listdir
from os.path import isfile, join
import subprocess

testdir = "tests"
testfiles = [f for f in listdir(testdir) if isfile(join(testdir, f)) and f.endswith(".iit")]

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
        else:
            print(f"{testfile}: passed")
