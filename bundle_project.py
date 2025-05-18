import os

ROOT_DIR = "."
APP_DIR = "app"
OUTPUT_FILE = "project_bundle.txt"

# Additional top-level files to include
ADDITIONAL_FILES = {
    "Makefile",
    "build.sh",
    "run.sh",
    "test.l1",
    "l1c.cabal",
    "test.s",
}


def collect_files():
    collected = []

    # Collect all files from app/
    for dirpath, _, filenames in os.walk(APP_DIR):
        for filename in filenames:
            full_path = os.path.join(dirpath, filename)
            rel_path = os.path.relpath(full_path, ROOT_DIR)
            collected.append((rel_path, full_path))

    # Collect specified root-level files
    for filename in ADDITIONAL_FILES:
        full_path = os.path.join(ROOT_DIR, filename)
        if os.path.exists(full_path):
            collected.append((filename, full_path))
        else:
            print(f"Warning: {filename} not found in root directory.")

    return collected


def write_bundle(files, output_path):
    with open(output_path, "w", encoding="utf-8") as out_file:
        for rel_path, full_path in files:
            out_file.write(f"\n\n===== FILE: {rel_path} =====\n\n")
            with open(full_path, "r", encoding="utf-8") as f:
                out_file.write(f.read())


if __name__ == "__main__":
    files = collect_files()
    write_bundle(files, OUTPUT_FILE)
    print(f"✅ Written {len(files)} files to {OUTPUT_FILE}")
