#[macro_export]
macro_rules! nameable {
  ( $( $T:ty ),* ) => {
    $(
      impl $crate::library::qualify::Nameable for $T {
        fn get_name(&self) -> String {
          self.name.clone()
        }
      }
    )*
  };
}
