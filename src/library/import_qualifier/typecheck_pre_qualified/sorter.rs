use crate::library::import_qualifier::Nameable;
use std::collections::HashMap;
use std::error::Error;

#[derive(Clone)]
pub enum Mark {
    None,
    Temp,
    Perm,
}

#[derive(Clone)]
pub struct MarkedNode<T> {
    pub mark: Mark,
    pub elem: T,
}

type GetInnerElemsCallback<T> =
    fn(HashMap<String, MarkedNode<T>>, &T) -> Vec<(String, T, Mark)>;

pub struct Sorter<T: Nameable + Clone> {
    marks: HashMap<String, MarkedNode<T>>,
    sorted: Vec<T>,
    get_inner_elems: GetInnerElemsCallback<T>,
}

impl<T: Nameable + Clone> Sorter<T> {
    pub fn create(
        es: Vec<T>,
        get_inner_elems: GetInnerElemsCallback<T>,
    ) -> Self {
        let marks = es
            .into_iter()
            .map(|e| {
                (
                    e.get_name(),
                    MarkedNode {
                        elem: e,
                        mark: Mark::None,
                    },
                )
            })
            .collect();
        Self {
            marks,
            sorted: Vec::new(),
            get_inner_elems,
        }
    }

    #[allow(unreachable_code)]
    pub fn sort(&mut self) -> Result<Vec<T>, Box<dyn Error>> {
        loop {
            let marks = self.marks.clone();
            let Some((name, MarkedNode { elem, mark })) = marks.iter().find(|m| {
                matches!(m.1.mark, Mark::None)
            }) else {
                break;
            };

            self.visit(name, elem, mark)?
        }

        Ok(self.sorted.clone())
    }

    fn visit(
        &mut self,
        name: &String,
        elem: &T,
        mark: &Mark,
    ) -> Result<(), Box<dyn Error>> {
        match mark {
            Mark::Perm => return Ok(()),
            Mark::Temp => return Err("cyclic reference found".into()),
            Mark::None => {
                let name = name.clone();
                let elem = elem.clone();

                self.marks.insert(
                    name.clone(),
                    MarkedNode {
                        elem: elem.clone(),
                        mark: Mark::Temp,
                    },
                );

                let marks = self.marks.clone();

                for (name, elem, mark) in (self.get_inner_elems)(marks, &elem)
                {
                    self.visit(&name, &elem, &mark)?
                }

                self.marks.insert(
                    name,
                    MarkedNode {
                        elem: elem.clone(),
                        mark: Mark::Perm,
                    },
                );

                self.sorted.push(elem)
            }
        };

        Ok(())
    }
}
