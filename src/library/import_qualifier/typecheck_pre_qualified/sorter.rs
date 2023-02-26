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
    fn(HashMap<String, MarkedNode<T>>, &T) -> Vec<(T, Mark)>;

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
            let Some(MarkedNode { elem, mark }) = marks.values()
                .find(|MarkedNode { mark, .. }| { matches!(mark, Mark::None) })
            else { break };

            self.visit(elem, mark)?
        }

        Ok(self.sorted.clone())
    }

    fn visit(&mut self, elem: &T, mark: &Mark) -> Result<(), Box<dyn Error>> {
        match mark {
            Mark::Perm => return Ok(()),
            Mark::Temp => return Err("cyclic reference found".into()),
            Mark::None => {
                let elem = elem.clone();

                self.marks.insert(
                    elem.get_name(),
                    MarkedNode {
                        elem: elem.clone(),
                        mark: Mark::Temp,
                    },
                );

                let marks = self.marks.clone();

                for (elem, mark) in (self.get_inner_elems)(marks, &elem) {
                    self.visit(&elem, &mark)?
                }

                self.marks.insert(
                    elem.get_name(),
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
