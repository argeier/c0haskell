#!/bin/bash
set -e

if [ $# -lt 2 ]; then
    echo "Usage: $0 <input-file> <output-file> [--ast-out <ast-file>] [--aasm-out <aasm-file>]"
    exit 1
fi

# Mandatory args
input_file=$1
output_file=$2
shift 2

# Optional flags
ast_out=""
aasm_out=""
while [[ $# -gt 0 ]]; do
    case "$1" in
        --ast-out)
            ast_out=$2
            shift 2
            ;;
        --aasm-out)
            aasm_out=$2
            shift 2
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Build the list of flags to pass to the compiler
compiler_flags=()
if [[ -n $ast_out ]]; then
    compiler_flags+=(--ast-out "$ast_out")
fi
if [[ -n $aasm_out ]]; then
    compiler_flags+=(--aasm-out "$aasm_out")
fi

echo "Running: cabal run l1c -- \"$input_file\" \"$output_file\" ${compiler_flags[*]}"
cabal run l1c -- "$input_file" "$output_file" "${compiler_flags[@]}"

if [[ -n $aasm_out ]]; then
    if [[ ! -f $aasm_out ]]; then
        echo "Error: Abstract assembly file not generated: $aasm_out"
        exit 1
    fi
fi

asm_file="${output_file}.s"
if [[ ! -f $asm_file ]]; then
    echo "Error: Assembly file not generated: $asm_file"
    exit 1
fi

# Link
echo "Compiling assembly to executable..."
gcc -o "$output_file" "$asm_file" -Wl,-z,noexecstack

echo "Successfully compiled: $output_file"
echo "You can run it with: ./$output_file"
