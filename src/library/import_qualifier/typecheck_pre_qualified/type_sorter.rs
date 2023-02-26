use crate::library::ast::untyped;
use crate::library::import_qualifier::typecheck_pre_qualified::sorter::{
    Mark, MarkedNode, Sorter,
};
use std::collections::HashMap;

fn get_inner_structs(
    marks: HashMap<String, MarkedNode<untyped::Struct>>,
    elem: &untyped::Struct,
) -> Vec<(untyped::Struct, Mark)> {
    let mut inner_structs = Vec::new();

    for (_, arg_type) in elem.args.clone() {
        let untyped::Type::Simple(struct_name) = arg_type else {
            continue
        };

        let Some(MarkedNode{ elem, mark }) = marks.get(struct_name.as_str()) else {
            continue
        };

        inner_structs.push((elem.clone(), mark.clone()))
    }

    inner_structs
}

pub fn get_type_sorter(ss: Vec<untyped::Struct>) -> Sorter<untyped::Struct> {
    Sorter::create(ss, get_inner_structs)
}

fn get_inner_vals(
    marks: HashMap<String, MarkedNode<untyped::ValDecl>>,
    val_decl: &untyped::ValDecl,
) -> Vec<(untyped::ValDecl, Mark)> {
    let mut inner_vals = Vec::new();

    for val_name in val_decl.inner_vals.clone() {
        let Some(MarkedNode { elem, mark}) = marks.get(val_name.as_str()) else {
            continue
        };

        inner_vals.push((elem.clone(), mark.clone()))
    }

    inner_vals
}

pub fn get_name_sorter(vs: Vec<untyped::ValDecl>) -> Sorter<untyped::ValDecl> {
    Sorter::create(vs, get_inner_vals)
}

#[cfg(test)]
mod test {
    use crate::library::ast::Type;
    use crate::library::import_qualifier::typecheck_pre_qualified::*;
    use rstest::*;

    #[rstest]
    fn test_sort() {
        // given
        let t1 = untyped::Struct {
            name: "T1".to_string(),
            args: Vec::from([
                ("a".to_string(), Type::Simple("Int".to_string())),
                ("b".to_string(), Type::Simple("T2".to_string())),
                ("c".to_string(), Type::Simple("Bool".to_string())),
            ]),
        };
        let t2 = untyped::Struct {
            name: "T2".to_string(),
            args: Vec::from([(
                "a".to_string(),
                Type::Simple("String".to_string()),
            )]),
        };
        let t3 = untyped::Struct {
            name: "T3".to_string(),
            args: Vec::from([
                ("a".to_string(), Type::Simple("Int".to_string())),
                ("b".to_string(), Type::Simple("T2".to_string())),
                ("c".to_string(), Type::Simple("String".to_string())),
            ]),
        };
        let structs = Vec::from([t1.clone(), t2.clone(), t3.clone()]);
        let mut gs = get_type_sorter(structs);

        // when
        let sorted = gs.sort().unwrap();

        // then
        let t1_pos = sorted.iter().position(|s| s.name == t1.name).unwrap();
        let t2_pos = sorted.iter().position(|s| s.name == t2.name).unwrap();
        let t3_pos = sorted.iter().position(|s| s.name == t3.name).unwrap();

        assert!(t2_pos < t1_pos);
        assert!(t2_pos < t3_pos);
    }
}
