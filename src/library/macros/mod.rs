#[macro_export]
macro_rules! nameable {
  ( $( $T:ty ),* ) => {
    use $crate::library::qualify::Nameable;

    $(
      impl Nameable for $T {
        fn get_name(&self) -> String {
          self.name.clone()
        }
      }
    )*
  };
}
