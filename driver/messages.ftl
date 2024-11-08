driver_required_input_files = input files are required
driver_required_output_files = output file is required
driver_unsupported_ext = extension `.{ $ext }` is not supported for input
driver_missing_ext = extension is missing in the input file: { $path }
    .note = the compiler does not recognize min-caml style arguments
    .help = use the real file path, e.g., `{ $path }.ml`

driver_cannot_start =
    cannot start the compiler session due to the above { $errs ->
        [one]   error
       *[other] {$errs} errors
    }
