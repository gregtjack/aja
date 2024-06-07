type Id = String;

#[derive(Debug)]
pub struct Prog {
    pub ds: Vec<Defn>,
    pub e: Expr,
}

#[derive(Debug)]
pub struct Defn {
    f: Id,
    xs: Vec<Id>,
    e: Expr,
}

#[derive(Debug)]
pub enum Expr {
    Eof,
    Empty,
    Int(u32),
    Bool(bool),
    Char(char),
    String(String),
    Prim0,
    Prim1,
}

