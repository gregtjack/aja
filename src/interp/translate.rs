use crate::ast::{Addr, Expr, IExpr, Id};

type LEnv = Vec<Id>;

/// Translates expressions to immediate expressions. 
/// Maps variables to their lexical address.
pub fn translate(e: Expr, mut r: LEnv) -> IExpr {
    match e {
        Expr::Eof => IExpr::Eof,
        Expr::Empty => IExpr::Empty,
        Expr::Literal(l) => IExpr::Literal(l),
        Expr::BinOp(lhs, op, rhs) => IExpr::BinOp(
            Box::new(translate(*lhs, r.clone())),
            op,
            Box::new(translate(*rhs, r)),
        ),
        Expr::Unary(op, e) => IExpr::Unary(op, Box::new(translate(*e, r))),
        Expr::Grouping(e) => IExpr::Grouping(Box::new(translate(*e, r))),
        Expr::If(e1, e2, e3) => IExpr::If(
            Box::new(translate(*e1, r.clone())),
            Box::new(translate(*e2, r.clone())),
            Box::new(translate(*e3, r)),
        ),
        Expr::Let(x, e1, e2) => IExpr::Let(
            Box::new(translate(*e1, r.clone())),
            Box::new(translate(*e2, {
                r.insert(0, x);
                r
            })),
        ),
        Expr::Var(id) => IExpr::Var(lexical_addr(id, r)),
        Expr::Call(_, _) => todo!(),
    }
}

fn lexical_addr(x: Id, r: LEnv) -> Addr {
    let mut addr = 0;
    for y in r {
        if x == y {
            break;
        } else {
            addr += 1;
        }
    }
    addr
}