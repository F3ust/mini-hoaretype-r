//Em viet dua tren docs + tham khao struct vai repo khac

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Plus,
    Minus,
    Times,
    Div,
    FloorDiv,
    Mod,
    Pow,
    Eq,
    Neq,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
    Is,
    IsNot,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnOp {
    Pos,
    Neg,
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Typ {
    TVar(String),
    TArrow(Box<Typ>, Box<Typ>),
    TConstructor(String, Vec<Typ>),
    TTuple(Vec<Typ>),
    TUnion(Box<Typ>, Box<Typ>),
    TIntersection(Box<Typ>, Box<Typ>),
    TNegation(Box<Typ>),
    TInt,
    TFloat,
    TString,
    TBool,
    TUnit,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    PVar(String),
    PConstructor(String, Vec<Pattern>),
    PTuple(Vec<Pattern>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expr(Expr),
    Let(String, Expr, Expr),
    Assign(String, Expr),
    Return(Expr),
    If(Expr, Vec<Stmt>, Vec<Stmt>),
    While(Expr, Vec<Stmt>),
    For(String, Expr, Vec<Stmt>),
    FunDef(String, Vec<String>, Vec<Stmt>),
    DataDef(String, Vec<(String, Vec<Typ>)>),
    Import(String),
    Block(Vec<Stmt>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Var(String),
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Null,
    Let(String, Box<Expr>, Box<Expr>),
    Lambda(Vec<String>, Box<Expr>),
    App(Box<Expr>, Vec<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    UnaryOp(UnOp, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Match(Box<Expr>, Vec<(Pattern, Expr)>),
    Tuple(Vec<Expr>),
    Record(Vec<(String, Expr)>),
    FieldAccess(Box<Expr>, String),
    Index(Box<Expr>, Box<Expr>),
    Constructor(String, Vec<Expr>),
    Block(Vec<Stmt>),
}

pub type Program = Vec<Stmt>;

fn pp_binop(op: &BinOp) -> &str {
    match op {
        BinOp::Plus => "+",
        BinOp::Minus => "-",
        BinOp::Times => "*",
        BinOp::Div => "/",
        BinOp::FloorDiv => "//",
        BinOp::Mod => "%",
        BinOp::Pow => "**",
        BinOp::Eq => "==",
        BinOp::Neq => "!=",
        BinOp::Lt => "<",
        BinOp::Le => "<=",
        BinOp::Gt => ">",
        BinOp::Ge => ">=",
        BinOp::And => "&&",
        BinOp::Or => "||",
        BinOp::Is => "is",
        BinOp::IsNot => "is not",
    }
}

fn pp_unop(op: &UnOp) -> &str {
    match op {
        UnOp::Pos => "+",
        UnOp::Neg => "-",
        UnOp::Not => "not ",
    }
}


fn show_pattern(p: &Pattern) -> String {
    match p {
        Pattern::PVar(x) => format!("PVar({})", x),
        Pattern::PConstructor(c, ps) => {
            let args = ps.iter().map(show_pattern).collect::<Vec<_>>().join(", ");
            format!("PConstructor({}, [{}])", c, args)
        }
        Pattern::PTuple(ps) => {
            let args = ps.iter().map(show_pattern).collect::<Vec<_>>().join(", ");
            format!("PTuple([{}])", args)
        }
    }
}

struct AstPrinter {
    indent_char: char,
}

impl AstPrinter {
    fn new() -> Self {
        AstPrinter { indent_char: ' ' }
    }

    fn get_indent(&self, indent: usize) -> String {
        std::iter::repeat(self.indent_char).take(indent).collect()
    }
    fn show_expr(&self, e: &Expr, indent: usize) -> String {
        let i_str = self.get_indent(indent);
        match e {
            Expr::Var(x) => format!("{}Var({})", i_str, x),
            Expr::Int(n) => format!("{}Int({})", i_str, n),
            Expr::Float(f) => format!("{}Float({:.2})", i_str, f),
            Expr::String(s) => format!("{}String(\"{}\")", i_str, s),
            Expr::Bool(b) => format!("{}Bool({})", i_str, b),
            Expr::Null => format!("{}Null", i_str),
            Expr::Let(x, e1, e2) => format!(
                "{}Let(\n{},\n{},\n{})",
                i_str,
                self.show_expr(&Expr::Var(x.clone()), indent + 2),
                self.show_expr(e1, indent + 2),
                self.show_expr(e2, indent + 2)
            ),
            Expr::Lambda(params, body) => format!(
                "{}Lambda([{}],\n{})",
                i_str,
                params.join(", "),
                self.show_expr(body, indent + 2)
            ),
            Expr::App(f, args) => {
                let args_str = args
                    .iter()
                    .map(|arg| self.show_expr(arg, indent + 2))
                    .collect::<Vec<_>>()
                    .join(",\n");
                format!("{}App(\n{},\n{}\n)", i_str, self.show_expr(f, indent + 2), args_str)
            }
            Expr::BinOp(op, e1, e2) => format!(
                "{}BinOp({},\n{},\n{})",
                i_str,
                pp_binop(op),
                self.show_expr(e1, indent + 2),
                self.show_expr(e2, indent + 2)
            ),
            Expr::UnaryOp(op, e) => format!(
                "{}UnaryOp({},\n{})",
                i_str,
                pp_unop(op),
                self.show_expr(e, indent + 2)
            ),
            Expr::If(c, t, e) => format!(
                "{}If(\n{},\n{},\n{})",
                i_str,
                self.show_expr(c, indent + 2),
                self.show_expr(t, indent + 2),
                self.show_expr(e, indent + 2)
            ),
            Expr::Match(e, cases) => {
                let cases_str = cases
                    .iter()
                    .map(|(p, expr)| {
                        format!(
                            "{}{} ->\n{}",
                            self.get_indent(indent + 2),
                            show_pattern(p),
                            self.show_expr(expr, indent + 4)
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(",\n");
                format!("{}Match(\n{},\n[\n{}\n{}])", i_str, self.show_expr(e, indent + 2), cases_str, i_str)
            }
            Expr::Tuple(es) => {
                let es_str = es
                    .iter()
                    .map(|expr| self.show_expr(expr, indent + 2))
                    .collect::<Vec<_>>()
                    .join(",\n");
                format!("{}Tuple([\n{}\n{}])", i_str, es_str, i_str)
            }
            Expr::Record(fields) => {
                let fields_str = fields
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, self.show_expr(v, indent + 2)))
                    .collect::<Vec<_>>()
                    .join(",\n");
                format!("{}Record([\n{}\n{}])", i_str, fields_str, i_str)
            }
            Expr::FieldAccess(e, f) => format!(
                "{}FieldAccess(\n{},\n{}{}\n)",
                i_str,
                self.show_expr(e, indent + 2),
                self.get_indent(indent + 2),
                f
            ),
            Expr::Index(e, i) => format!(
                "{}Index(\n{},\n{})",
                i_str,
                self.show_expr(e, indent + 2),
                self.show_expr(i, indent + 2)
            ),
            Expr::Constructor(c, args) => {
                let args_str = args
                    .iter()
                    .map(|arg| self.show_expr(arg, indent + 2))
                    .collect::<Vec<_>>()
                    .join(",\n");
                format!("{}Constructor({},\n[\n{}\n{}])", i_str, c, args_str, i_str)
            }
            Expr::Block(stmts) => {
                let stmts_str = stmts
                    .iter()
                    .map(|stmt| self.show_stmt(stmt, indent + 2))
                    .collect::<Vec<_>>()
                    .join(",\n");
                format!("{}Block([\n{}\n{}])", i_str, stmts_str, i_str)
            }
        }
    }
    
    fn show_stmt(&self, s: &Stmt, indent: usize) -> String {
        let i_str = self.get_indent(indent);
        match s {
            Stmt::Expr(e) => format!("{}Expr(\n{}\n)", i_str, self.show_expr(e, indent + 2)),
            Stmt::Let(x, e1, e2) => format!(
                "{}Let(\n{},\n{},\n{})",
                i_str,
                self.show_expr(&Expr::Var(x.clone()), indent + 2),
                self.show_expr(e1, indent + 2),
                self.show_expr(e2, indent + 2)
            ),
            Stmt::Assign(x, e) => format!(
                "{}Assign({},\n{})",
                i_str,
                x,
                self.show_expr(e, indent + 2)
            ),
            Stmt::Return(e) => format!("{}Return(\n{}\n)", i_str, self.show_expr(e, indent + 2)),
            Stmt::If(cond, t, e) => {
                let t_str = t.iter().map(|s| self.show_stmt(s, indent + 2)).collect::<Vec<_>>().join(",\n");
                let e_str = e.iter().map(|s| self.show_stmt(s, indent + 2)).collect::<Vec<_>>().join(",\n");
                format!(
                    "{}If(\n{},\n[\n{}\n],\n[\n{}\n])",
                    i_str,
                    self.show_expr(cond, indent + 2),
                    t_str,
                    e_str
                )
            }
            Stmt::While(cond, body) => {
                let body_str = body.iter().map(|s| self.show_stmt(s, indent + 2)).collect::<Vec<_>>().join(",\n");
                format!(
                    "{}While(\n{},\n[\n{}\n])",
                    i_str,
                    self.show_expr(cond, indent + 2),
                    body_str
                )
            }
            Stmt::For(v, iter, body) => {
                let body_str = body.iter().map(|s| self.show_stmt(s, indent + 2)).collect::<Vec<_>>().join(",\n");
                format!(
                    "{}For({},\n{},\n[\n{}\n])",
                    i_str,
                    v,
                    self.show_expr(iter, indent + 2),
                    body_str
                )
            }
            Stmt::FunDef(name, params, body) => {
                let body_str = body.iter().map(|s| self.show_stmt(s, indent + 2)).collect::<Vec<_>>().join(",\n");
                format!(
                    "{}FunDef({}, [{}],\n[\n{}\n])",
                    i_str,
                    name,
                    params.join(", "),
                    body_str
                )
            }
            Stmt::DataDef(name, fields) => {
                let fields_str = fields.iter().map(|(k, _)| k.clone()).collect::<Vec<_>>().join(", ");
                format!("{}DataDef({}, [{}])", i_str, name, fields_str)
            }
            Stmt::Import(m) => format!("{}Import({})", i_str, m),
            Stmt::Block(stmts) => {
                let stmts_str = stmts
                    .iter()
                    .map(|stmt| self.show_stmt(stmt, indent + 2))
                    .collect::<Vec<_>>()
                    .join(",\n");
                format!("{}Block([\n{}\n{}])", i_str, stmts_str, i_str)
            }
        }
    }
}

pub fn show_program(prog: &Program) -> String {
    let printer = AstPrinter::new();
    prog.iter()
        .map(|stmt| printer.show_stmt(stmt, 0))
        .collect::<Vec<String>>()
        .join("\n")
}