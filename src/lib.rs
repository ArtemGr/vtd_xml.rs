// [build] cargo test -- --nocapture

/// Handmade FFI bindings to the C library (ximpleware_2.12).
pub mod sys {
  pub enum VTDGen {}

  #[link(name="vtdxml")] extern {
    pub fn createVTDGen() -> *mut VTDGen;
    pub fn freeVTDGen (vtd_gen: *mut VTDGen);}}

#[cfg(test)] mod tests {
  #[test] fn it_works() {
    use ::sys::*;
    let vtd_gen = unsafe {createVTDGen()};
    println! ("vtd_gen: {:?}", vtd_gen);
    unsafe {freeVTDGen (vtd_gen)};}}
