from os import listdir
from os.path import isfile, join
from subprocess import PIPE, Popen


testdir = "./tests"
testfiles = [f for f in listdir(testdir) if isfile(join(testdir, f)) and f.endswith(".iit")]

for testfile in testfiles:
    with open(join(testdir, testfile)) as f:
        if "synerr" not in testfile:
            lines = f.readlines()
            expected_result = lines[0].split(":")[1].strip("\n").strip(" ")
        else:
            expected_result = ""
        process = Popen(["./iitran", join(testdir, testfile)], stdout=PIPE, stderr=None)
        result, error = process.communicate()
        if error:
            raise Exception(f"{testfile}: encountered error {error}")
        result = result.decode("utf-8").strip("\n").strip(" ")
        if result != expected_result:
            raise Exception(f"{testfile}: expected result to be '{expected_result}', got '{result}'")
        else:
            print(f"{testfile}: passed")
