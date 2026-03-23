use extendr_api::prelude::*;
use polars::prelude::*;
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

/// Read a CSV file using Polars (10-30x faster than read.csv).
/// @param path Character. Path to the CSV file.
/// @param n_rows Integer or NULL. Max rows to read.
/// @export
#[extendr]
fn rust_read_csv(path: &str, n_rows: Nullable<i32>) -> List {
    ensure_polars_init();
    let start = Instant::now();

    let mut reader = LazyCsvReader::new(path);
    if let NotNull(n) = n_rows {
        reader = reader.with_n_rows(Some(n as usize));
    }

    let df = reader
        .finish()
        .expect("Failed to parse CSV")
        .collect()
        .expect("Failed to collect DataFrame");

    let elapsed = start.elapsed().as_secs_f64();
    df_to_list(&df, elapsed)
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
        .expect("Failed to read Parquet")
        .collect()
        .expect("Failed to collect DataFrame");

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
        .finish()
        .expect("Failed to read CSV")
        .collect()
        .expect("Failed to collect");

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
        .finish()
        .expect("Failed to read CSV");

    let agg_expr = match agg_fn {
        "mean" => col(agg_col).mean(),
        "sum" => col(agg_col).sum(),
        "count" => col(agg_col).count(),
        "min" => col(agg_col).min(),
        "max" => col(agg_col).max(),
        "std" => col(agg_col).std(1),
        _ => panic!("Unknown agg: {}", agg_fn),
    };

    let result_col = format!("{}_{}", agg_col, agg_fn);
    let df = lazy
        .group_by([col(group_col)])
        .agg([agg_expr.alias(&result_col)])
        .collect()
        .expect("Failed to aggregate");

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
        .finish()
        .expect("Failed to read CSV");

    let filter_expr = match op {
        "gt" => col(col_name).gt(lit(value)),
        "lt" => col(col_name).lt(lit(value)),
        "gte" => col(col_name).gt_eq(lit(value)),
        "lte" => col(col_name).lt_eq(lit(value)),
        "eq" => col(col_name).eq(lit(value)),
        "neq" => col(col_name).neq(lit(value)),
        _ => panic!("Unknown op: {}", op),
    };

    let df = lazy
        .filter(filter_expr)
        .collect()
        .expect("Failed to filter");

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
        .finish()
        .expect("Failed to read CSV");

    let df = lazy
        .sort([by], SortMultipleOptions::new().with_order_descending(descending))
        .collect()
        .expect("Failed to sort");

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

extendr_module! {
    mod quasar;
    fn rust_read_csv;
    fn rust_read_parquet;
    fn rust_describe;
    fn rust_group_agg;
    fn rust_filter;
    fn rust_sort;
}
