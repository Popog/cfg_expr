mod token;
mod parse;

pub use parse::{Cfg,CfgExpr};

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
