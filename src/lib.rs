// [build] cargo test -- --nocapture

extern crate encoding;
extern crate libc;

// Bindgen example:
// cd /tmp && git clone https://github.com/ArtemGr/vtd_xml.rs.git && cd vtd_xml.rs && cargo build
// cd target/debug/build/vtd_xml-*/out/ximpleware-2.12-c/vtd
// bindgen autoPilot.h | less

/// Handmade FFI bindings to the C library (ximpleware_2.12).
pub mod sys {
  use libc::c_int;

  pub enum VTDGen {}
  pub enum VTDNav {}
  pub enum AutoPilot {}

  pub type UByte = u8;
  pub type UCSChar = i16;

  #[derive(Copy, Clone, PartialEq)]
  #[repr(u32)]
  #[derive(Debug)]
  pub enum Bool {FALSE = 0, TRUE = 1}
  pub type Boolean = Bool;

  #[link(name="vtdxml")] extern {

    // vtdGen.h

    pub fn createVTDGen() -> *mut VTDGen;
    pub fn freeVTDGen (vg: *mut VTDGen);
    /// Set the XMLDoc container.
    pub fn setDoc (vg: *mut VTDGen, byteArray: *const UByte, arrayLen: c_int);
    /// Generating VTD tokens and Location cache info.
    /// One specifies whether the parsing is namespace aware or not.
    pub fn parse (vg: *mut VTDGen, ns: Boolean);
    /// Returns the VTDNav object after parsing, it also cleans 
    /// internal state so VTDGen can process the next file.
    pub fn getNav (vg: *mut VTDGen) -> *mut VTDNav;
    pub fn freeVTDNav_shim (vn: *mut VTDNav);

    // autoPilot.h

    pub fn createAutoPilot (v: *mut VTDNav) -> *mut AutoPilot;
    pub fn createAutoPilot2() -> *mut AutoPilot;
    pub fn freeAutoPilot (ap: *mut AutoPilot);
    /// Declare prefix/URL binding.
    pub fn declareXPathNameSpace (ap: *mut AutoPilot, prefix: *const UCSChar, URL: *const UCSChar);
    /// This function selects the string representing XPath expression
    /// Usually evalXPath is called afterwards
    /// return true is the XPath is valid
    pub fn selectXPath (ap: *mut AutoPilot, s: *const UCSChar) -> Boolean;
    pub fn bind (ap: *mut AutoPilot, vn: *mut VTDNav);
    pub fn evalXPath (ap: *mut AutoPilot) -> c_int;
    /// Convert a token at the given index to a String, (entities and char references resolved).
    /// An attribute name or an element name will get the UCS2 string of qualified name.
    pub fn toString (vn: *mut VTDNav, index: c_int) -> *mut UCSChar;
    /// This function returns of the token index of the type character data or CDATA.
    /// Notice that it is intended to support data orient XML (not mixed-content XML).
    pub fn getText (vn: *mut VTDNav) -> c_int;
    /// This method normalizes a token into a string in a way that resembles DOM.
    /// The leading and trailing white space characters will be stripped.
    /// The entity and character references will be resolved
    /// Multiple whitespaces char will be collapsed into one.
    pub fn toNormalizedString (vn: *mut VTDNav, index: c_int) -> *mut UCSChar;}}

pub mod helpers {
  use encoding::{Encoding, DecoderTrap};
  use encoding::all::UTF_16LE;
  use super::sys::UCSChar;
  use std::mem::size_of;
  use std::ptr::null;
  use std::slice::from_raw_parts;

  /// Decodes a NIL-terminated UCSChar into a UTF-8 string.
  pub fn usc2string (us: *const UCSChar) -> String {
    if us == null() {return String::new()}
    let mut len = 0;
    while unsafe {*us.offset (len)} != 0 {len += 1}
    let slice = unsafe {from_raw_parts (us as *const u8, len as usize * size_of::<UCSChar>())};
    UTF_16LE.decode (slice, DecoderTrap::Ignore) .expect ("!UCS")}}

#[cfg(test)] mod tests {
  use libc::{self, c_void};

  #[test] fn rss_reader() {  // http://vtd-xml.sourceforge.net/codeSample/cs2.html, RSSReader.c
    use ::sys::*;
    use ::helpers::*;
    use encoding::{Encoding, EncoderTrap};
    use encoding::all::UTF_16LE;
    use libc::c_int;

    let vg = unsafe {createVTDGen()};
    //let xml = "<foo><bar surname=\"Stover\">Smokey</bar></foo>";
    // http://vtd-xml.sourceforge.net/codeSample/servers.xml
    let xml = "\
      <?xml version=\"1.0\" encoding=\"UTF-8\"?><rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" \
          xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:sy=\"http://purl.org/rss/1.0/modules/syndication/\" \
          xmlns:rss=\"http://purl.org/rss/1.0/\" xmlns=\"http://purl.org/rss/1.0/\">\
        <item rdf:about=\"http://www.nwfusion.com/news/2004/0614amd.html\">\
          <title>AMD readies dual-core Opteron</title>\
          <link>http://www.nwfusion.com/news/2004/0614amd.html</link>\
          <description>AMD Monday reiterated plans to ship dual-core Opteron processors</description>\
          <dc:creator>Jennifer Mears</dc:creator>\
          <dc:date>2004-06-14T00:00:00Z</dc:date>\
        </item>\
      </rdf:RDF>";
    unsafe {setDoc (vg, xml.as_ptr(), xml.len() as c_int)};
    unsafe {parse (vg, Bool::TRUE)};
    let vn = unsafe {getNav (vg)};
    let ap = unsafe {createAutoPilot2()};

    let mut ns1 = UTF_16LE.encode ("ns1", EncoderTrap::Ignore) .expect ("!encode");
    ns1.push (0); ns1.push (0);
    let mut purl11 = UTF_16LE.encode ("http://purl.org/dc/elements/1.1/", EncoderTrap::Ignore) .expect ("!encode");
    purl11.push (0); purl11.push (0);
    unsafe {declareXPathNameSpace (ap, ns1.as_ptr() as *const i16, purl11.as_ptr() as *const i16)};

    let mut xpath = UTF_16LE.encode ("//ns1:*", EncoderTrap::Ignore) .expect ("!encode");
    xpath.push (0); xpath.push (0);
    if unsafe {selectXPath (ap, xpath.as_ptr() as *const i16)} == Bool::TRUE {
      unsafe {bind (ap, vn)};
      let mut result; while {result = unsafe {evalXPath (ap)}; result} != -1 {
        let tmp_string = unsafe {toString (vn, result)};
        let name = usc2string (tmp_string);
        unsafe {libc::free (tmp_string as *mut c_void)};
        let t = unsafe {getText (vn)};
        let mut text = String::new();
        if t != -1 {
          let tmp_string = unsafe {toNormalizedString (vn, t)};
          text = usc2string (tmp_string);
          unsafe {libc::free (tmp_string as *mut c_void)}}
        println! ("evalXPath result: {}; name: {}; text: {}", result, name, text);}}
    unsafe {freeVTDNav_shim (vn)};
    unsafe {freeVTDGen (vg)};
    unsafe {freeAutoPilot (ap)};}}
