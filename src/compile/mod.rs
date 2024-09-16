mod llvm;
mod qbe;

pub enum Target {
    QBE,
}

pub trait Generator {
    fn generate(&mut self);
}
