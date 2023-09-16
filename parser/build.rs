fn main() {
    #[cfg(feature = "lalrpop")]
    lalrpop::Configuration::new()
        .generate_in_source_tree()
        .process()
        .unwrap();
}
