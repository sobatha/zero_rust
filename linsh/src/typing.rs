use crate::parser;
use std::{borrow::Cow, cmp::Ordering, collections::BTreeMap, mem};

type VarToType = BTreeMap<String, Option<parser::TypeExpr>>;
type TResult<'a> = Result<parser::TypeExpr, Cow<'a, str>>;

pub fn typing<'a>(expr: &parser::Expr, env: &mut TypeEnv, depth: usize) -> TResult<'a> {
    match expr {
        parser::Expr::App(e) => typing_app(e, env, depth),
        parser::Expr::QVal(e) => typing_qval(e, env, depth),
        parser::Expr::Free(e) => typing_free(e, env, depth),
        parser::Expr::If(e) => typing_if(e, env, depth),
        parser::Expr::Split(e) => typing_split(e, env, depth),
        parser::Expr::Var(e) => typing_var(e, env),
        parser::Expr::Let(e) => typing_let(e, env, depth),
    }
}

fn typing_let<'a>(expr: &parser::LetExpr, env: &mut TypeEnv, depth: usize) -> TResult<'a> {
    let var_type = typing(&expr.expr1, env, depth)?;

    if var_type != expr.ty {
        return Err("let expression does not match the type".into());
    };

    let depth = depth + 1;

    env.push(depth);
    env.insert(expr.var.clone(), var_type); // 変数の型をinsert
    let t2 = typing(&expr.expr2, env, depth)?;

    // lin型の変数を消費しているかチェック
    let (elin, _) = env.pop(depth);
    for (k, v) in elin.unwrap().iter() {
        if v.is_some() {
            return Err(format!("let式内でlin型の変数\"{k}\"を消費していない").into());
        }
    }

    return Ok(t2);
}

fn typing_var<'a>(expr: &str, env: &mut TypeEnv) -> TResult<'a> {
    if let Some(it) = env.get_mut(expr) {
        if let Some(t) = it {
            if t.qual == parser::Qual::Lin {
                let eret = t.clone();
                *it = None;
                return Ok(eret);
            } else {
                return Ok(t.clone());
            }
        }
    }
    Err(format!("{expr} is not defined, or already used Lin, or cannot be captured").into())
}

fn typing_split<'a>(expr: &parser::SplitExpr, env: &mut TypeEnv, depth: usize) -> TResult<'a> {
    let pair: parser::TypeExpr = typing(&expr.expr, env, depth)?;
    let depth = depth + 1;
    match pair.prim {
        parser::PrimType::Pair(x, y) => {
            env.push(depth);
            env.insert(expr.left.clone(), *x);
            env.insert(expr.right.clone(), *y);
        }
        _ => {
            return Err("in split expression must return pair type".into());
        }
    }

    let ret = typing(&expr.body, env, depth);

    let (elin, _) = env.pop(depth);

    for (k, v) in elin.unwrap().iter() {
        if v.is_some() {
            return Err(format!("does not consume lin type {k} in split expr").into());
        }
    }

    ret
}

fn typing_if<'a>(expr: &parser::IfExpr, env: &mut TypeEnv, depth: usize) -> TResult<'a> {
    let t1 = typing(&expr.cond_expr, env, depth)?;
    if t1.prim != parser::PrimType::Bool {
        return Err("if conditonal expression must be Bool".into());
    }

    let mut e = env.clone();
    let t2 = typing(&expr.then_expr, &mut e, depth)?;
    let t3 = typing(&expr.else_expr, env, depth)?;

    if t2 != t3 || e != *env {
        return Err("if then and else must be the same type".into());
    }

    Ok(t2)
}

fn typing_free<'a>(expr: &parser::FreeExpr, env: &mut TypeEnv, depth: usize) -> TResult<'a> {
    if let Some((_, t)) = env.env_lin.get_mut(&expr.var) {
        if t.is_some() {
            *t = None;
            return typing(&expr.expr, env, depth);
        }
    }
    Err(format!(
        "既にfreeしたか、lin型ではない変数\"{}\"をfreeしている",
        expr.var
    )
    .into())
}

fn typing_qval<'a>(expr: &parser::QValExpr, env: &mut TypeEnv, depth: usize) -> TResult<'a> {
    let p = match &expr.val {
        parser::ValExpr::Bool(_) => parser::PrimType::Bool,
        parser::ValExpr::Pair(e1, e2) => {
            let t1 = typing(e1, env, depth)?;
            let t2 = typing(e2, env, depth)?;

            if expr.qual == parser::Qual::Un
                && (t1.qual == parser::Qual::Lin || t2.qual == parser::Qual::Lin)
            {
                return Err("It's prohibited to use lin inside un type".into());
            }

            parser::PrimType::Pair(Box::new(t1), Box::new(t2))
        }
        parser::ValExpr::Fun(e) => {
            let env_prev = if expr.qual == parser::Qual::Un {
                Some(mem::take(&mut env.env_lin))
            } else {
                None
            };

            let depth = depth + 1;
            env.push(depth);
            env.insert(e.var.clone(), e.ty.clone());

            let t = typing(&e.expr, env, depth)?;

            let (elin, _) = env.pop(depth);
            for (k, v) in elin.unwrap().iter() {
                if v.is_some() {
                    return Err(
                        format!("haven't consumed lin variable in function definition").into(),
                    );
                }
            }

            if let Some(ep) = env_prev {
                env.env_lin = ep;
            }

            parser::PrimType::Arrow(Box::new(e.ty.clone()), Box::new(t))
        }
    };
    Ok(parser::TypeExpr {
        qual: expr.qual.clone(),
        prim: p,
    })
}

fn typing_app<'a>(e: &parser::AppExpr, env: &mut TypeEnv, depth: usize) -> TResult<'a> {
    todo!()
}

#[derive(Debug, Clone, Eq, PartialEq, Default)]
struct TypeEnvStack {
    vars: BTreeMap<usize, VarToType>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TypeEnv {
    env_lin: TypeEnvStack,
    env_un: TypeEnvStack,
}

impl TypeEnv {
    pub fn new() -> Self {
        Self {
            env_lin: TypeEnvStack::new(),
            env_un: TypeEnvStack::new(),
        }
    }

    fn push(&mut self, depth: usize) {
        self.env_lin.push(depth);
        self.env_un.push(depth);
    }

    fn pop(&mut self, depth: usize) -> (Option<VarToType>, Option<VarToType>) {
        let t1 = self.env_lin.pop(depth);
        let t2 = self.env_un.pop(depth);
        (t1, t2)
    }

    fn insert(&mut self, key: String, value: parser::TypeExpr) {
        if value.qual == parser::Qual::Lin {
            self.env_lin.insert(key, value)
        } else {
            self.env_un.insert(key, value)
        }
    }

    fn get_mut(&mut self, key: &str) -> Option<&mut Option<parser::TypeExpr>> {
        match (self.env_lin.get_mut(key), self.env_un.get_mut(key)) {
            (Some((d1, t1)), Some((d2, t2))) => match d1.cmp(&d2) {
                Ordering::Less => Some(t2),
                Ordering::Greater => Some(t1),
                Ordering::Equal => panic!("invalid type environment"),
            },
            (Some((_, t1)), None) => Some(t1),
            (None, Some((_, t2))) => Some(t2),
            _ => None,
        }
    }
}

impl TypeEnvStack {
    pub fn new() -> TypeEnvStack {
        TypeEnvStack {
            vars: BTreeMap::new(),
        }
    }

    pub fn push(&mut self, depth: usize) {
        self.vars.insert(depth, BTreeMap::new());
    }

    pub fn pop(&mut self, depth: usize) -> Option<VarToType> {
        self.vars.remove(&depth)
    }

    pub fn insert(&mut self, key: String, value: parser::TypeExpr) {
        if let Some(last) = self.vars.iter_mut().next_back() {
            last.1.insert(key, Some(value));
        }
    }

    pub fn get_mut(&mut self, key: &str) -> Option<(usize, &mut Option<parser::TypeExpr>)> {
        for (depth, elm) in self.vars.iter_mut().rev() {
            if let Some(e) = elm.get_mut(key) {
                return Some((*depth, e));
            }
        }
        None
    }
}
