// use re
use regex::Regex;
use std::fmt::Display;
use std::io;
use std::rc::Rc;
use std::fmt;

fn main() {
    println!("Enter your expression:");
    let mut raw = String::new();
    io::stdin().read_line(&mut raw)
        .expect("failed to read line");
    let expr = prune(parse(&raw));
    println!("got {}", expr);
    println!("simplified {}", prune(simplify(expr)))
}

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Sum(Vec<Expr>),
    Product(Vec<Expr>),
    Atom(Rc<str>),
    // Exp(Expr, Expr),
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Sum(terms) => {
                write!(f, "(")?;
                for (i, term) in terms.iter().enumerate() {
                    if i > 0 {
                        write!(f, " + ")?;
                    }
                    write!(f, "{}", term)?;
                }
                write!(f, ")")
            }
            Expr::Product(factors) => {
                for factor in factors.iter() {
                    write!(f, "{}", factor)?;
                }
                Ok(())
            }
            Expr::Atom(atom) => write!(f, "{}", atom),
            // Handle Exp(Expr, Expr) case if needed
            // Expr::Exp(lhs, rhs) => write!(f, "Exp({}, {})", lhs, rhs),
        }
    }
}

fn simplify(e: Expr) -> Expr {
    match e {
        Expr::Sum(l) => {
            Expr::Sum(l.into_iter().map(simplify).collect())
        },
        Expr::Product(mut l) => {
            let b = l.pop().unwrap();
            let a = l.pop().unwrap();
            let mut ret = Vec::new();
            match (&a,&b) {
                (Expr::Sum(al), Expr::Sum(bl)) => {
                    for ax in al.iter() {
                        for bx in bl.iter() {
                            ret.push(Expr::Product(vec![ax.clone(), bx.clone()]));
                        }
                    }
                },
                (Expr::Sum(al), Expr::Atom(_)) => {
                    for ax in al {
                        ret.push(Expr::Product(vec![ax.clone(), b.clone()]))
                    }
                },
                (Expr::Atom(_), Expr::Sum(bl)) => {
                    for bx in bl {
                        ret.push(Expr::Product(vec![a.clone(), bx.clone()]))
                    }
                },
                (Expr::Atom(_), Expr::Atom(_)) => {
                    ret.push(Expr::Product(vec![a.clone(), b.clone()]));
                },
                (_, _) => panic!("simplifying product inside a product, need to prune"),
            };
            if l.len() == 0 {
                Expr::Sum(ret)
            } else {
                l.push(Expr::Sum(ret));
                simplify(Expr::Product(l))
            }
        },
        Expr::Atom(a) => Expr::Atom(a),
    }
}

fn prune(e: Expr) -> Expr {
    match e {
        Expr::Sum(mut l) => if l.len() == 1 {
            prune(l.swap_remove(0))
        } else {
            let ret = l.into_iter().fold(Vec::new(), |mut acc, x| match x {
                Expr::Sum(inner_l) => {acc.extend(inner_l); acc},
                a => {acc.push(a); acc},
            });
            Expr::Sum(ret.into_iter().map(prune).collect())
        },
        Expr::Product(mut l) => if l.len() == 1 {
            prune(l.swap_remove(0))
        } else {
            let ret = l.into_iter().fold(Vec::new(), |mut acc, x| match x {
                Expr::Product(inner_l) => {acc.extend(inner_l); acc},
                a => {acc.push(a); acc},
            });
            Expr::Product(ret.into_iter().map(prune).collect())
        },
        Expr::Atom(a) => Expr::Atom(a),
    }
}

// PEMDAS -- this is how we construct exprs weakest to strongest binding
fn parse(raw: &str) -> Expr {
    let (expr, the_rest) = parse_sum(raw.trim());
    assert!(the_rest.len() == 0);
    expr
}

// we can handle subtraction as multiplication by negative 1
fn parse_sum(raw: &str) -> (Expr, Rc<str>) {
    let (expr, mut the_rest) = parse_product(raw);
    let mut l = vec!(expr);
    let re = Regex::new(r"^\s*\+\s*(.*)").unwrap();

    while the_rest.len() != 0 {
        if let Some(cap) = re.captures(&the_rest) {
            the_rest = Rc::from(&cap[1]);
        } else {
            break;
        }
        let (expr, r) = parse_product(&the_rest);
        the_rest = r;
        l.push(expr);
    }
    (Expr::Sum(l), the_rest)
}

fn parse_product(raw: &str) -> (Expr, Rc<str>) {
    let (expr, mut the_rest) = parse_paren(raw);
    let mut l = vec!(expr);
    // TODO: frail
    let re = Regex::new(r"^(\s*\*\s*)?([^+ \t].*)").unwrap();

    while the_rest.len() != 0 {
        if let Some(cap) = re.captures(&the_rest) {
            the_rest = Rc::from(&cap[2]);
        } else {
            break
        }
        let (expr, r) = parse_paren(&the_rest);
        the_rest = r;
        l.push(expr);
    }
    (Expr::Product(l), Rc::from(the_rest))
}

fn parse_paren(raw: &str) -> (Expr, Rc<str>) {
    let re = Regex::new(r"\(([^)]*)\)(.*)").unwrap();
    if let Some(cap) = re.captures(raw) {
        println!("recurse: `{}`", &cap[1]);
        let (expr, rest) = parse_sum(&cap[1]);
        assert!(rest.len() == 0);
        (expr, Rc::from(&cap[2]))
    } else {
        parse_atom(raw)
    }
}

fn parse_atom(raw: &str) -> (Expr, Rc<str>) {
    let re = Regex::new(r"^([0-9]+|[a-z])(.*)").unwrap();
    if let Some(cap) = re.captures(raw) {
        (Expr::Atom(Rc::from(&cap[1])), Rc::from(&cap[2]))
    } else {
        // TODO: error
        (Expr::Atom(Rc::from("")), Rc::from(raw))
    }
}

// // fn parse_exp(raw: String):
// //     return parse_p(raw)


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        assert_eq!(prune(parse("1 + 2")), Expr::Sum(vec!(Expr::Atom(Rc::from("1")), Expr::Atom(Rc::from("2")))));
        assert_eq!(prune(parse("1 + 2 + 3")), Expr::Sum(vec!(Expr::Atom(Rc::from("1")), Expr::Atom(Rc::from("2")), Expr::Atom(Rc::from("3")))));
        assert_eq!(prune(parse("1 + 2 * 3")), Expr::Sum(vec!(Expr::Atom(Rc::from("1")), Expr::Product(vec!(Expr::Atom(Rc::from("2")), Expr::Atom(Rc::from("3")))))));
    }
}
