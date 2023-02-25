use crate::library::ast::untyped;
use crate::library::import_qualifier::typecheck_pre_qualified::sorter::{
    Mark, MarkedNode, Sorter,
};
use std::collections::HashMap;

fn type_sorter_get_inner_elems(
    marks: HashMap<String, MarkedNode<untyped::Struct>>,
    elem: &untyped::Struct,
) -> Vec<(String, untyped::Struct, Mark)> {
    let mut inner_elems = Vec::new();

    for (_, t) in elem.args.clone() {
        let untyped::Type::Simple(tn) = t else {
            continue
        };

        let Some(MarkedNode{ elem, mark }) = marks.get(tn.as_str()) else {
            continue
        };

        inner_elems.push((tn, elem.clone(), mark.clone()))
    }

    inner_elems
}

pub fn get_type_sorter(ss: Vec<untyped::Struct>) -> Sorter<untyped::Struct> {
    Sorter::create(ss, type_sorter_get_inner_elems)
}

fn name_sorter_get_inner_elems(
    marks: HashMap<String, MarkedNode<untyped::ValDecl>>,
    elem: &untyped::ValDecl,
) -> Vec<(String, untyped::ValDecl, Mark)> {
    let mut inner_elems = Vec::new();

    for inner_val in elem.inner_vals.clone() {
        let Some(MarkedNode { elem, mark}) = marks.get(inner_val.as_str()) else {
            continue
        };

        inner_elems.push((inner_val, elem.clone(), mark.clone()))
    }

    inner_elems
}

pub fn get_name_sorter(vs: Vec<untyped::ValDecl>) -> Sorter<untyped::ValDecl> {
    Sorter::create(vs, name_sorter_get_inner_elems)
}

#[cfg(test)]
mod tests {
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
