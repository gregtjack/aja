use std::io::Write;

use crate::ast::Program;

use super::Generator;
use qbe;

struct CEnvironment;

pub struct QbeGenerator {
    pub program: Program,
    c_env: CEnvironment,
}

impl Generator for QbeGenerator {
    fn generate(&mut self) {
        let module = qbe::Module::new();

        for defn in &self.program.ds {}
    }
}
