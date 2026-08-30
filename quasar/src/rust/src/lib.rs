use extendr_api::prelude::*;
use polars::prelude::*;
use std::io::{Read, Write};
use std::sync::Once;
use std::time::Instant;

// ============================================================
// QUASAR Rust Backend — Polars-powered data operations
// ============================================================

// Initialize Polars thread pool ONCE to prevent segfaults
// when calling multiple Rust functions in the same R session.
// Rayon's thread pool conflicts with R's single-threaded GC
// if re-initialized across calls.
static INIT: Once = Once::new();

fn ensure_polars_init() {
    INIT.call_once(|| {
        // Limit threads to avoid race conditions with R's GC.
        // Even single-threaded, Polars is 5-15x faster than read.csv
        // thanks to its columnar engine and zero-copy parsing.
        if std::env::var("POLARS_MAX_THREADS").is_err() {
            std::env::set_var("POLARS_MAX_THREADS", "1");
        }
    });
}

// R writes missing values as "NA" (and sometimes empty); tell the Polars CSV
// reader to treat both as null so R-exported CSVs parse cleanly on typed columns.
fn na_nulls() -> Option<NullValues> {
    Some(NullValues::AllColumns(vec!["NA".into(), "".into()]))
}

/// Read a CSV file using Polars (10-30x faster than read.csv).
/// @param path Character. Path to the CSV file.
/// @param n_rows Integer or NULL. Max rows to read.
/// @param separator Character. Field delimiter (first byte is used, e.g. "|", ",", or a tab).
/// @param columns Character vector or NULL. Columns to read; when given, Polars pushes
///   the projection down to the scan and only reads those columns from disk.
/// @export
#[extendr]
fn rust_read_csv(
    path: &str,
    n_rows: Nullable<i32>,
    separator: &str,
    columns: Nullable<Vec<String>>,
) -> List {
    ensure_polars_init();
    let start = Instant::now();

    let sep = separator.as_bytes().first().copied().unwrap_or(b',');
    let mut reader = LazyCsvReader::new(path)
        .with_separator(sep)
        .with_null_values(na_nulls());
    if let NotNull(n) = n_rows {
        reader = reader.with_n_rows(Some(n as usize));
    }

    let mut lazy = reader
        .finish()
        .unwrap_or_else(|e| panic!("QUASAR: Failed to parse CSV '{}': {}", path, e));

    // Projection pushdown: Polars reads ONLY these columns from disk. This is the
    // memory win on wide files -- selecting before collect() prunes the scan.
    if let NotNull(cols) = columns {
        if !cols.is_empty() {
            let exprs: Vec<Expr> = cols.iter().map(|c| col(c.as_str())).collect();
            lazy = lazy.select(exprs);
        }
    }

    let df = lazy
        .collect()
        .unwrap_or_else(|e| panic!("QUASAR: Failed to collect CSV '{}': {}", path, e));

    let elapsed = start.elapsed().as_secs_f64();
    df_to_list(&df, elapsed)
}

/// Stream a delimited text file to Parquet without materialising it in memory.
///
/// Uses the Polars streaming engine: rows flow scan -> (projection) -> Parquet
/// writer in bounded memory, so files far larger than RAM convert with a flat
/// footprint. This is the primitive behind a "bronze" layer for huge inputs.
///
/// @param path Character. Source file (already UTF-8; transcode upstream if needed).
/// @param out Character. Destination Parquet path.
/// @param separator Character. Field delimiter (first byte is used).
/// @param columns Character vector or NULL. Columns to keep (projection pushdown).
/// @return Character. Elapsed seconds (as a string).
/// @export
#[extendr]
fn rust_sink_parquet(
    path: &str,
    out: &str,
    separator: &str,
    columns: Nullable<Vec<String>>,
) -> String {
    ensure_polars_init();
    let start = Instant::now();

    let sep = separator.as_bytes().first().copied().unwrap_or(b',');
    let mut lazy = LazyCsvReader::new(path)
        .with_separator(sep)
        .with_null_values(na_nulls())
        .finish()
        .unwrap_or_else(|e| panic!("QUASAR: Failed to scan CSV '{}': {}", path, e));

    if let NotNull(cols) = columns {
        if !cols.is_empty() {
            let exprs: Vec<Expr> = cols.iter().map(|c| col(c.as_str())).collect();
            lazy = lazy.select(exprs);
        }
    }

    let out_path = std::path::PathBuf::from(out);
    lazy.with_streaming(true)
        .sink_parquet(&out_path, ParquetWriteOptions::default(), None)
        .unwrap_or_else(|e| panic!("QUASAR: Failed to sink Parquet '{}': {}", out, e));

    format!("{:.3}", start.elapsed().as_secs_f64())
}

/// Read a Parquet file using Polars.
/// @param path Character. Path to the Parquet file.
/// @export
#[extendr]
fn rust_read_parquet(path: &str) -> List {
    ensure_polars_init();
    let start = Instant::now();

    let args = ScanArgsParquet::default();
    let df = LazyFrame::scan_parquet(path, args)
        .unwrap_or_else(|e| panic!("QUASAR: Failed to read Parquet '{}': {}", path, e))
        .collect()
        .unwrap_or_else(|e| panic!("QUASAR: Failed to collect Parquet '{}': {}", path, e));

    let elapsed = start.elapsed().as_secs_f64();
    df_to_list(&df, elapsed)
}

/// Fast summary statistics for numeric columns.
/// @param path Character. Path to CSV file.
/// @export
#[extendr]
fn rust_describe(path: &str) -> List {
    ensure_polars_init();
    let df = LazyCsvReader::new(path)
        .with_null_values(na_nulls())
        .finish()
        .unwrap_or_else(|e| panic!("QUASAR: Failed to read CSV '{}': {}", path, e))
        .collect()
        .unwrap_or_else(|e| panic!("QUASAR: Failed to collect CSV '{}': {}", path, e));

    let col_names: Vec<String> = df.get_column_names().iter().map(|s| s.to_string()).collect();

    let mut stat_names: Vec<String> = Vec::new();
    let mut counts: Vec<f64> = Vec::new();
    let mut means: Vec<f64> = Vec::new();
    let mut stds: Vec<f64> = Vec::new();
    let mut mins: Vec<f64> = Vec::new();
    let mut maxs: Vec<f64> = Vec::new();

    for name in &col_names {
        let series = df.column(name.as_str()).unwrap().as_materialized_series();
        if series.dtype().is_primitive_numeric() {
            stat_names.push(name.clone());
            let f64s = series.cast(&DataType::Float64).unwrap();
            let ca = f64s.f64().unwrap();
            counts.push(ca.len() as f64 - ca.null_count() as f64);
            means.push(ca.mean().unwrap_or(f64::NAN));
            stds.push(ca.std(1).unwrap_or(f64::NAN));
            mins.push(ca.into_iter().flatten().fold(f64::INFINITY, f64::min));
            maxs.push(ca.into_iter().flatten().fold(f64::NEG_INFINITY, f64::max));
        }
    }

    list!(
        column = stat_names,
        count = counts,
        mean = means,
        std = stds,
        min = mins,
        max = maxs
    )
    .into()
}

/// Fast group-by aggregation.
/// @param path Character. Path to CSV file.
/// @param group_col Character. Column to group by.
/// @param agg_col Character. Column to aggregate.
/// @param agg_fn Character. One of: mean, sum, count, min, max, std.
/// @export
#[extendr]
fn rust_group_agg(path: &str, group_col: &str, agg_col: &str, agg_fn: &str) -> List {
    ensure_polars_init();
    let start = Instant::now();

    let lazy = LazyCsvReader::new(path)
        .with_null_values(na_nulls())
        .finish()
        .unwrap_or_else(|e| panic!("QUASAR: Failed to read CSV '{}': {}", path, e));

    let agg_expr = match agg_fn {
        "mean" => col(agg_col).mean(),
        "sum" => col(agg_col).sum(),
        "count" => col(agg_col).count(),
        "min" => col(agg_col).min(),
        "max" => col(agg_col).max(),
        "std" => col(agg_col).std(1),
        _ => panic!("QUASAR: Unknown aggregation '{}'. Use: mean, sum, count, min, max, std", agg_fn),
    };

    let result_col = format!("{}_{}", agg_col, agg_fn);
    let df = lazy
        .group_by([col(group_col)])
        .agg([agg_expr.alias(&result_col)])
        .collect()
        .unwrap_or_else(|e| panic!("QUASAR: Failed to aggregate '{}' by '{}': {}", agg_col, group_col, e));

    let elapsed = start.elapsed().as_secs_f64();
    df_to_list(&df, elapsed)
}

/// Fast filter rows.
/// @param path Character. Path to CSV file.
/// @param col_name Character. Column to filter on.
/// @param op Character. One of: gt, lt, gte, lte, eq, neq.
/// @param value Double. Value to compare against.
/// @export
#[extendr]
fn rust_filter(path: &str, col_name: &str, op: &str, value: f64) -> List {
    ensure_polars_init();
    let start = Instant::now();

    let lazy = LazyCsvReader::new(path)
        .with_null_values(na_nulls())
        .finish()
        .unwrap_or_else(|e| panic!("QUASAR: Failed to read CSV '{}': {}", path, e));

    let filter_expr = match op {
        "gt" => col(col_name).gt(lit(value)),
        "lt" => col(col_name).lt(lit(value)),
        "gte" => col(col_name).gt_eq(lit(value)),
        "lte" => col(col_name).lt_eq(lit(value)),
        "eq" => col(col_name).eq(lit(value)),
        "neq" => col(col_name).neq(lit(value)),
        _ => panic!("QUASAR: Unknown operator '{}'. Use: gt, lt, gte, lte, eq, neq", op),
    };

    let df = lazy
        .filter(filter_expr)
        .collect()
        .unwrap_or_else(|e| panic!("QUASAR: Failed to filter '{}' {} {}: {}", col_name, op, value, e));

    let elapsed = start.elapsed().as_secs_f64();
    df_to_list(&df, elapsed)
}

/// Fast sort rows.
/// @param path Character. Path to CSV file.
/// @param by Character. Column to sort by.
/// @param descending Logical.
/// @export
#[extendr]
fn rust_sort(path: &str, by: &str, descending: bool) -> List {
    ensure_polars_init();
    let start = Instant::now();

    let lazy = LazyCsvReader::new(path)
        .with_null_values(na_nulls())
        .finish()
        .unwrap_or_else(|e| panic!("QUASAR: Failed to read CSV '{}': {}", path, e));

    let df = lazy
        .sort([by], SortMultipleOptions::new().with_order_descending(descending))
        .collect()
        .unwrap_or_else(|e| panic!("QUASAR: Failed to sort by '{}': {}", by, e));

    let elapsed = start.elapsed().as_secs_f64();
    df_to_list(&df, elapsed)
}

// ============================================================
// Internal: DataFrame → R List conversion
// ============================================================

fn df_to_list(df: &DataFrame, elapsed: f64) -> List {
    let col_names: Vec<String> = df.get_column_names().iter().map(|s| s.to_string()).collect();
    let nrows = df.height() as i32;

    let mut r_vecs: Vec<Robj> = col_names
        .iter()
        .map(|name| {
            let series = df.column(name.as_str()).unwrap();
            series_to_robj(series)
        })
        .collect();

    r_vecs.push(r!(elapsed));
    r_vecs.push(r!(nrows));

    let mut all_names: Vec<String> = col_names;
    all_names.push("_elapsed_secs".to_string());
    all_names.push("_nrows".to_string());

    let mut result = List::from_values(r_vecs);
    let name_refs: Vec<&str> = all_names.iter().map(|s| s.as_str()).collect();
    let _ = result.set_names(name_refs);
    result
}

fn series_to_robj(series: &Column) -> Robj {
    let s = series.as_materialized_series();
    match s.dtype() {
        DataType::Float64 => {
            let v: Vec<f64> = s.f64().unwrap().into_iter()
                .map(|x| x.unwrap_or(f64::NAN)).collect();
            r!(v)
        }
        DataType::Float32 => {
            let v: Vec<f64> = s.f32().unwrap().into_iter()
                .map(|x| x.map(|f| f as f64).unwrap_or(f64::NAN)).collect();
            r!(v)
        }
        DataType::Int64 => {
            let v: Vec<f64> = s.i64().unwrap().into_iter()
                .map(|x| x.map(|i| i as f64).unwrap_or(f64::NAN)).collect();
            r!(v)
        }
        DataType::Int32 => {
            let v: Vec<i32> = s.i32().unwrap().into_iter()
                .map(|x| x.unwrap_or(i32::MIN)).collect();
            r!(v)
        }
        DataType::Boolean => {
            let v: Vec<Rbool> = s.bool().unwrap().into_iter()
                .map(|x| match x {
                    Some(true) => Rbool::from(true),
                    Some(false) => Rbool::from(false),
                    None => Rbool::na(),
                }).collect();
            r!(v)
        }
        DataType::String => {
            let v: Vec<String> = s.str().unwrap().into_iter()
                .map(|x| x.unwrap_or("NA").to_string()).collect();
            r!(v)
        }
        _ => {
            let v: Vec<String> = (0..s.len())
                .map(|i| format!("{}", s.get(i).unwrap())).collect();
            r!(v)
        }
    }
}

/// Transcode a single-byte-encoded text file to a UTF-8 copy (streaming, flat memory).
///
/// Handles latin1 / ISO-8859-1 / windows-1252 correctly (accents and n-tilde survive) and
/// **strips embedded NUL bytes** instead of aborting -- so a partially corrupt file still
/// yields its readable rows. Replaces the R-level `rawToChar` loop, which is ~80x slower on
/// large files and throws on the first embedded NUL. Single-byte encodings never split a code
/// point across a chunk boundary, so per-chunk decoding is exact.
///
/// @param path Character. Source file (single-byte encoding).
/// @param out Character. Destination UTF-8 file.
/// @param from Character. Source encoding label, e.g. "latin1" (falls back to windows-1252).
/// @return Character. Elapsed seconds (as a string).
/// @export
// Decode one byte of a single-byte Latin-family encoding to a Unicode scalar.
// `win1252 = false` -> pure ISO-8859-1 (byte value == code point). `true` -> windows-1252,
// which differs only in 0x80..=0x9F (typographic punctuation, euro, etc.). Hand-rolled to
// avoid pulling an external crate into the vendored build.
#[inline]
fn decode_byte(b: u8, win1252: bool) -> char {
    if win1252 {
        match b {
            0x80 => '\u{20AC}', 0x82 => '\u{201A}', 0x83 => '\u{0192}', 0x84 => '\u{201E}',
            0x85 => '\u{2026}', 0x86 => '\u{2020}', 0x87 => '\u{2021}', 0x88 => '\u{02C6}',
            0x89 => '\u{2030}', 0x8A => '\u{0160}', 0x8B => '\u{2039}', 0x8C => '\u{0152}',
            0x8E => '\u{017D}', 0x91 => '\u{2018}', 0x92 => '\u{2019}', 0x93 => '\u{201C}',
            0x94 => '\u{201D}', 0x95 => '\u{2022}', 0x96 => '\u{2013}', 0x97 => '\u{2014}',
            0x98 => '\u{02DC}', 0x99 => '\u{2122}', 0x9A => '\u{0161}', 0x9B => '\u{203A}',
            0x9C => '\u{0153}', 0x9E => '\u{017E}', 0x9F => '\u{0178}',
            _ => char::from(b), // ASCII, 0xA0..=0xFF, and the 5 undefined slots (as C1 controls)
        }
    } else {
        char::from(b)
    }
}

#[extendr]
fn rust_transcode_utf8(path: &str, out: &str, from: &str) -> String {
    let start = Instant::now();
    // "latin1"/"iso-8859-1" -> pure Latin-1; anything else -> windows-1252 (its superset).
    let f = from.to_ascii_lowercase();
    let win1252 = !(f.contains("8859-1") || f == "latin1" || f == "l1" || f.contains("iso88591"));
    let fin = std::fs::File::open(path)
        .unwrap_or_else(|e| panic!("QUASAR: cannot open '{}': {}", path, e));
    let fout = std::fs::File::create(out)
        .unwrap_or_else(|e| panic!("QUASAR: cannot create '{}': {}", out, e));
    let mut reader = std::io::BufReader::with_capacity(1 << 20, fin);
    let mut writer = std::io::BufWriter::with_capacity(1 << 20, fout);
    let mut buf = vec![0u8; 1 << 20];
    let mut enc = [0u8; 4];
    loop {
        let n = reader
            .read(&mut buf)
            .unwrap_or_else(|e| panic!("QUASAR: read error on '{}': {}", path, e));
        if n == 0 {
            break;
        }
        let mut out_buf: Vec<u8> = Vec::with_capacity(n + n / 2);
        for &b in &buf[..n] {
            if b == 0 {
                continue; // embedded NUL is never valid in text; drop it instead of aborting
            }
            let s = decode_byte(b, win1252).encode_utf8(&mut enc);
            out_buf.extend_from_slice(s.as_bytes());
        }
        writer
            .write_all(&out_buf)
            .unwrap_or_else(|e| panic!("QUASAR: write error on '{}': {}", out, e));
    }
    writer.flush().ok();
    format!("{:.3}", start.elapsed().as_secs_f64())
}

extendr_module! {
    mod rquasar;
    fn rust_read_csv;
    fn rust_sink_parquet;
    fn rust_read_parquet;
    fn rust_transcode_utf8;
    fn rust_describe;
    fn rust_group_agg;
    fn rust_filter;
    fn rust_sort;
}
