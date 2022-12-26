use crate::library::ast::untyped;
use crate::library::import_qualifier::typecheck_pre_qualified::TypeCheckResult;
use std::collections::HashMap;

#[derive(Clone)]
enum Mark {
    None,
    Temp,
    Perm,
}

pub struct TypeSorter {
    marks: HashMap<String, (untyped::Struct, Mark)>,
    sorted: Vec<untyped::Struct>,
}

impl TypeSorter {
    pub fn create(ss: Vec<untyped::Struct>) -> Self {
        let marks = ss
            .into_iter()
            .map(|s| (s.name.clone(), (s, Mark::None)))
            .collect();
        Self {
            marks,
            sorted: Vec::new(),
        }
    }

    #[allow(unreachable_code)]
    pub fn sort(&mut self) -> TypeCheckResult<Vec<untyped::Struct>> {
        loop {
            let marks = self.marks.clone();
            let Some((n, (s, m))) = marks.iter().find(|m| {
                matches!(m.1.1, Mark::None)
            }) else {
                break;
            };

            self.visit(n, s, m)?
        }

        Ok(self.sorted.clone())
    }

    fn visit(
        &mut self,
        n: &String,
        s: &untyped::Struct,
        m: &Mark,
    ) -> TypeCheckResult<()> {
        match m {
            Mark::Perm => return Ok(()),
            Mark::Temp => return Err(String::from("test").into()),
            Mark::None => {
                self.marks.insert((*n).clone(), ((*s).clone(), Mark::Temp));

                let marks = self.marks.clone();

                for (_, t) in s.clone().args {
                    let untyped::Type::Simple(tn) = t else {
                        continue
                    };

                    let Some((s, m)) = marks.get(tn.as_str()) else {
                        continue
                    };

                    self.visit(&tn, s, m)?
                }

                self.marks.insert((*n).clone(), ((*s).clone(), Mark::Perm));

                self.sorted.push((*s).clone())
            }
        };

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::library::ast::Type;
    use crate::library::import_qualifier::typecheck_pre_qualified::*;
    use rstest::*;
    use std::ops::Index;

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
        let mut gs = TypeSorter::create(structs);

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
