use kaon::KaonError;

mod test_runner;

fn main() -> Result<(), KaonError> {
    test_runner::test_snippets()
}
