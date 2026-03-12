import subprocess
import sys
import time
import glob
import os
import re

def run_solver(mode, filepath):
    start = time.time()
    result = subprocess.run(
        ["cabal", "run", "sat-solver", "--", f"--{mode}", "--verify", filepath],
        capture_output=True, text=True
    )
    end = time.time() - start
    output = result.stdout + result.stderr
    invalid = "invalid" in output.lower()

    return end, not invalid

def main():
    if len(sys.argv) != 3:
        print(f"Usage: {sys.argv[0]} <directory> <count>")
        sys.exit(1)

    directory = sys.argv[1]
    count = int(sys.argv[2])

    # the way the test files are named causes them to be run in the
    # wrong order (e.g., #1 then #10 then #100, etc.), so I used regex
    def natural_key(path):
        return [int(s) if s.isdigit() else s.lower()
                for s in re.split(r'(\d+)', os.path.basename(path))]

    files = sorted(glob.glob(os.path.join(directory, "*.cnf")), key=natural_key)[:count]
    if not files:
        print(f"No .cnf files in {directory}.")
        sys.exit(1)

    pure_total = 0.0
    mutable_total = 0.0
    failures = []

    for f in files:
        name = os.path.basename(f)

        pure_time, pure_sat = run_solver("pure", f)
        pure_total += pure_time
        if not pure_sat:
            failures.append(f"Pure failed on {name}")

        mutable_time, mutable_sat = run_solver("mutable", f)
        mutable_total += mutable_time
        if not mutable_sat:
            failures.append(f"Mutable failed on {name}")

        print(f"{name}: pure {pure_time:.2f}s; mutable {mutable_time:.2f}s")

    print()
    print(f"Pure total: {pure_total:.2f}s")
    print(f"Mutable total: {mutable_total:.2f}s")

    if failures:
        print()
        print("Verification failures:")
        for message in failures:
            print(message)
    else:
        print("All solutions verified correctly.")

if __name__ == "__main__":
    main()
