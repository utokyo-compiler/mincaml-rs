fn main() {
    #[cfg(feature = "lalrpop")]
    lalrpop::process_root().unwrap();
}
