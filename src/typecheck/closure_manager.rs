use crate::ast::typed::*;

pub struct ClosureManager {
    closures: Vec<Closure>,
}

impl ClosureManager {
    pub fn new() -> Self {
        Self {
            closures: Vec::new(),
        }
    }

    fn get_tail(&mut self) -> &mut Closure {
        if self.closures.is_empty() {
            self.closures.push(Vec::new())
        }

        self.closures.last_mut().unwrap()
    }

    pub fn push_layer(&mut self) {
        self.closures.push(Vec::new());
    }

    pub fn pop_layer(&mut self) {
        self.closures.pop();
    }

    pub fn append(&mut self, mut closure: Closure) {
        let tail = self.get_tail();
        tail.append(&mut closure)
    }

    pub fn clone(&self) -> Closure {
        let result = self.closures.clone();

        result.into_iter().flatten().collect()
    }

    pub fn push(&mut self, name: String, type_: Type) {
        let tail = self.get_tail();
        tail.push((name, type_))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::*;

    #[fixture]
    fn cm() -> ClosureManager {
        ClosureManager::new()
    }

    #[rstest]
    fn test_append(mut cm: ClosureManager) {
        // given
        let closure = Vec::from([
            ("a".to_string(), Type::Simple(SimpleType::Int)),
            ("b".to_string(), Type::Simple(SimpleType::Float)),
            ("c".to_string(), Type::Simple(SimpleType::Bool)),
        ]);
        let expected_closures = Vec::from([closure.clone()]);

        // when
        cm.append(closure);

        // then
        assert_eq!(cm.closures, expected_closures)
    }
}
