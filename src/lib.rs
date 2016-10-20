// [build] cargo test unicode -- --nocapture

/*
 * Copyright (C) 2016 Artem Grinblat
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#![feature(test)]

extern crate antidote;
extern crate encoding;
#[macro_use] extern crate lazy_static;
extern crate libc;
extern crate test;

use antidote::Mutex;
use libc::c_void;
use std::any::Any;
use std::ffi::CStr;
use std::fmt::{self, Display};
use std::mem::transmute;
use std::panic::catch_unwind;
use std::ptr::null;

// Bindgen example:
// cd /tmp && git clone https://github.com/ArtemGr/vtd_xml.rs.git && cd vtd_xml.rs && cargo build
// cd target/debug/build/vtd_xml-*/out/ximpleware-2.12-c/vtd
// bindgen autoPilot.h | less

/// Handmade FFI bindings to the C library (ximpleware_2.12).
pub mod sys {
  use libc::{c_int, c_char, c_void, wchar_t, size_t};
  use std::default::Default;
  use std::mem::zeroed;

  pub enum VTDGen {}
  /// The VTD Navigator allows one to navigate XML document represented in VTD records and Location caches.
  /// There is one and only one cursor that you can navigate to any part of the tree.
  pub enum VTDNav {}
  pub enum AutoPilot {}

  pub type UByte = u8;
  pub type UCSChar = wchar_t;  // NB: 2 bytes on Windows, 4 bytes on Linux/GCC.

  #[derive(Copy, Clone, PartialEq)]
  #[repr(u32)]
  #[derive(Debug)]
  pub enum Bool {FALSE = 0, TRUE = 1}
  pub type Boolean = Bool;

  #[derive(Copy, Clone)]
  #[repr(u32)]
  #[derive(Debug)]
  pub enum Direction {
    Root = 0,
    Parent = 1,
    FirstChild = 2,
    LastChild = 3,
    NextSibling = 4,
    PrevSibling = 5}

  #[derive(Copy, Clone)]
  #[repr(u32)]
  #[derive(Debug)]
  pub enum ExceptionType {
    OutOfMem = 0,
    InvalidArgument = 1,
    ArrayOutOfBound = 2,
    ParseException = 3,
    NavException = 4,
    PilotException = 5,
    NumberFormatException = 6,
    XPathParseException = 7,
    XPathEvalException = 8,
    ModifyException = 9,
    IndexWriteException = 10,
    IndexReadException = 11,
    IoException = 12,
    TranscodeException = 13,
    OtherException = 14}
  #[repr(C)]
  #[derive(Copy, Clone)]
  #[derive(Debug)]
  pub struct VtdException {
    pub et: ExceptionType,
    pub subtype: c_int,
    pub msg: *const c_char,
    pub sub_msg: *const c_char}
  impl Default for VtdException {
    fn default() -> Self {unsafe {zeroed()}}}

  #[link(name="vtdxml")] extern {

    // vtdGen.h

    pub fn createVTDGen() -> *mut VTDGen;
    pub fn freeVTDGen (vg: *mut VTDGen);
    /// Set the XMLDoc container.
    pub fn setDoc (vg: *mut VTDGen, byteArray: *const UByte, arrayLen: c_int);
    /// Generating VTD tokens and Location cache info.
    /// One specifies whether the parsing is namespace aware or not.
    pub fn parse (vg: *mut VTDGen, ns: Boolean);
    /// Generating VTD tokens and Location cache info for an XML file.
    ///
    /// Throws no exceptions (cf. https://ximpleware.wordpress.com/2016/06/02/parsefile-vs-parse-a-quick-comparison/).
    pub fn parseFile (vg: *mut VTDGen, ns: Boolean, fileName: *const u8) -> Boolean;
    pub fn selectLcDepth (vg: *mut VTDGen, d: c_int);
    /// Returns the VTDNav object after parsing, it also cleans 
    /// internal state so VTDGen can process the next file.
    pub fn getNav (vg: *mut VTDGen) -> *mut VTDNav;
    /// Clones the cursor.
    /// With ximpleware-2.12-c it doesn't seem to work (the cloned cursor isn't navigable).
    /// Use `getCurrentIndex_shim` and `recoverNode_shim` instead.
    /// Or `duplicateNav_shim` with `recoverNode_shim`.
    pub fn cloneNav_shim (vn: *mut VTDNav) -> *mut VTDNav;
    pub fn duplicateNav_shim (vn: *mut VTDNav) -> *mut VTDNav;
    /// This method takes a vtd index, and recover its correspondin node position, the index can only be of node type element,
    /// document, attribute name, attribute value or character data, or CDATA. */
    pub fn recoverNode_shim (vn: *mut VTDNav, index: c_int);
    pub fn freeVTDNav_shim (vn: *mut VTDNav);

    // vtdNav.h

    /// Get the depth (>=0) of the current element.
    pub fn getCurrentDepth (vn: *mut VTDNav) -> c_int;
    /// The index of the element under the cursor. Can be used with `toString` to fetch the element name.
    pub fn getCurrentIndex_shim (vn: *mut VTDNav) -> c_int;
    /// Move the cursor to the element according to the direction constants If no such element, no position change and return false.
    pub fn toElement_shim (vn: *mut VTDNav, direction: Direction) -> Boolean;
    /// Move the cursor to the element according to the direction constants and the element name.
    /// If no such element, no position change and return false.
    /// "*" matches any element.
    pub fn toElement2_shim (vn: *mut VTDNav, direction: Direction, en: *const UCSChar) -> Boolean;
    /// Test if the current element matches the given name.
    pub fn matchElement (vn: *mut VTDNav, en: *const UCSChar) -> Boolean;
    /// Return the attribute count of the element at the cursor position.
    pub fn getAttrCount (vn: *mut VTDNav) -> c_int;
    /// Test whether current element has an attribute with the matching name.
    pub fn hasAttr (vn: *mut VTDNav, attrName: *const UCSChar) -> Boolean;
    /// Get the token index of the attribute value given an attribute name.
    /// Returns -1 if the attribute wasn't found.
    pub fn getAttrVal (vn: *mut VTDNav, attrName: *const UCSChar) -> c_int;

    // autoPilot.h

    pub fn createAutoPilot (v: *mut VTDNav) -> *mut AutoPilot;
    pub fn createAutoPilot2() -> *mut AutoPilot;
    pub fn freeAutoPilot (ap: *mut AutoPilot);
    /// Declare prefix/URL binding.
    /// NB: `prefix` and `url` aren't *copied*, so they must live as long as `AutoPilot` lives.
    pub fn declareXPathNameSpace (ap: *mut AutoPilot, prefix: *const UCSChar, url: *const UCSChar);
    /// This function selects the string representing XPath expression.
    /// Usually `evalXPath` is called afterwards.
    /// Return `True` is the XPath is valid.
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
    /// The entity and character references will be resolved.
    /// Multiple whitespaces char will be collapsed into one.
    pub fn toNormalizedString (vn: *mut VTDNav, index: c_int) -> *mut UCSChar;

    // shims.c

    /// Returns 0 on success and 1 if VTD exception was raised.
    pub fn vtd_try_catch_shim (cb: extern fn (*mut c_void, *mut c_void),
                               closure_pp: *mut c_void, panic_p: *mut c_void, ex: *mut VtdException) -> c_int;}

  // --- iconv -------

  pub type Iconv = *mut c_void;
  extern {
    pub fn iconv_open_shim (tocode: *const u8, fromcode: *const u8) -> Iconv;
    /// Convert at most *INBYTESLEFT bytes from *INBUF according to the code conversion algorithm specified by CD
    /// and place up to OUTBYTESLEFT bytes in buffer at *OUTBUF.
    pub fn iconv_shim (cd: Iconv, inbuf: *mut *const u8, inbytesleft: *mut size_t,
                       outbuf: *mut *mut u8, outbytesleft: *mut size_t) -> size_t;
    pub fn iconv_close_shim (cd: Iconv) -> c_int;
    pub fn is_errno_einval() -> c_int;
    pub fn is_errno_e2big() -> c_int;}}

pub mod helpers {
  use ::sys::{iconv_open_shim, iconv_shim, iconv_close_shim, is_errno_e2big, UCSChar, Iconv};
  use libc::{self, size_t, c_void};
  use std::mem::{uninitialized, size_of};
  use std::ptr::null;
  use std::str::from_utf8_unchecked;

  // TODO: Implement a proper encoder/decoder (or find a way to reuse one).
  // cf. http://stackoverflow.com/questions/6240055/manually-converting-unicode-codepoints-into-utf-8-and-utf-16
  // http://stackoverflow.com/questions/33642339/how-is-a-high-unicode-codepoint-expressed-as-two-codepoints
  // http://stackoverflow.com/questions/38349372/convert-codepoint-to-utf-8-byte-array-in-java-using-shifting-operations

  struct IconvSync (Iconv);
  unsafe impl Sync for IconvSync {}
  impl Drop for IconvSync {
    fn drop (&mut self) {
      if unsafe {iconv_close_shim (self.0)} != 0 {panic! ("!iconv_close")}}}

  lazy_static! {static ref WCHAR_T_TO_UTF_8: IconvSync = {
    let cd = unsafe {iconv_open_shim ("UTF-8\0".as_ptr(), "WCHAR_T\0".as_ptr())};
    if cd as size_t == size_t::max_value() {panic! ("!iconv_open")}
    IconvSync (cd)};}

  /// Decodes a NIL-terminated `wchar_t` string into a UTF-8 Rust string. WIP.
  pub fn ucs2string<'a> (sbuf: &'a mut String, ucs: *const UCSChar, free: bool) -> &'a String {
    sbuf.clear();
    if ucs == null() {return sbuf}

    let mut wide_len = 0;
    loop {
      let ch = unsafe {*ucs.offset (wide_len)};
      if ch == 0 {break}
      wide_len += 1;}

    // https://www.gnu.org/software/libc/manual/html_node/iconv-Examples.html
    let cd = WCHAR_T_TO_UTF_8.0;
    let mut buf: [u8; 1024] = unsafe {uninitialized()};
    let mut ucs_p: *const u8 = ucs as *const u8;
    let mut ucs_len = wide_len as usize * size_of::<UCSChar>();
    loop {
      let mut buf_p: *mut u8 = buf.as_mut_ptr();
      let mut buf_len = 1024;
      let rc = unsafe {iconv_shim (cd, &mut ucs_p, &mut ucs_len, &mut buf_p, &mut buf_len)};
      let mut finished = true;
      if rc == size_t::max_value() {
        if unsafe {is_errno_e2big()} == 1 {  // `buf` is full.
          finished = false;
        } else {panic! ("!iconv")}}
      let encoded_len = buf_p as usize - buf.as_ptr() as usize;
      let encoded = unsafe {from_utf8_unchecked (&buf[0..encoded_len])};
      sbuf.reserve (encoded_len);
      sbuf.push_str (encoded);
      if finished {break}}

    if free {unsafe {libc::free (ucs as *mut c_void)}}
    sbuf}

  /// Converts a UTF-8 string into a NIL-terminated `wchar_t` string. WIP.
  pub fn str2ucs<'a> (buf: &'a mut Vec<UCSChar>, s: &str) -> &'a Vec<UCSChar> {
    buf.clear();
    buf.reserve (s.len() + 1);
    for ch in s.chars() {buf.push (ch as UCSChar)}
    buf.push (0);  // NIL-terminated.
    buf}}

#[derive(Debug)]
pub struct VtdError {
  pub et: sys::ExceptionType,
  pub subtype: i32,
  pub msg: String,
  pub sub_msg: String}
impl Display for VtdError {
  fn fmt (&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {write! (fmt, "{:?}", *self)}}

// C version *should* be thread-safe, but I seem to be seing an EOF issue when using VTD from multiple threads.
// "Parse exception in getChar", "Premature EOF reached, XML document incomplete".
// With a global lock it never happens.
lazy_static! {static ref LOCK: Mutex<()> = Mutex::new (());}

/// Catches VTD-XML exceptions, returning them as a Rust error.
///
/// Rust panics in the `cb` are propagated.
pub fn vtd_catch (mut cb: &mut FnMut()) -> Result<(), VtdError> {
  // http://stackoverflow.com/a/38997480/257568.
  let closure_pp: *mut c_void = unsafe {transmute (&mut cb)};
  let mut ex = sys::VtdException::default();
  let mut panic = String::new();
  let panic_p: *mut c_void = unsafe {transmute (&mut panic)};
  let lock = LOCK.lock();
  if unsafe {::sys::vtd_try_catch_shim (::vtd_xml_try_catch_rust_shim, closure_pp, panic_p, &mut ex)} == 0 {
    if !panic.is_empty() {panic! ("vtd_catch] {}", panic)}
    test::black_box (&lock);
    Ok (())
  } else {
    let err = VtdError {
      et: ex.et,
      subtype: ex.subtype as i32,
      msg: if ex.msg == null() {String::new()} else {
        unsafe {CStr::from_ptr (ex.msg as *mut i8)} .to_string_lossy().into_owned()},
      sub_msg: if ex.sub_msg == null() {String::new()} else {
        unsafe {CStr::from_ptr (ex.sub_msg as *mut i8)} .to_string_lossy().into_owned()}};
    test::black_box (&lock);
    Err (err)}}

/// Useful with panic handlers.
fn any_to_str<'a> (message: &'a Box<Any + Send + 'static>) -> Option<&'a str> {
  if let Some (message) = message.downcast_ref::<&str>() {return Some (message)}
  if let Some (message) = message.downcast_ref::<String>() {return Some (&message[..])}
  return None}

/// Called from inside a C function (`vtd_try_catch_shim`) in order to execute some Rust code while catching any VTD-XML exceptions.
#[no_mangle] #[doc(hidden)]
pub extern "C" fn vtd_xml_try_catch_rust_shim (closure_pp: *mut c_void, panic_p: *mut c_void) {
  if let Err (panic) = catch_unwind (|| {
    let closure: &mut &mut FnMut() = unsafe {transmute (closure_pp)};
    closure();}) {
      let message = match any_to_str (&panic) {Some (s) => s, None => "panic in vtd_catch"};
      let panic_buf: &mut String = unsafe {transmute (panic_p)};
      panic_buf.push_str (message);}}

#[cfg(test)] mod tests {
  use ::sys::*;
  use ::helpers::*;
  use ::vtd_catch;
  use libc::{self, c_int};
  use std;
  use std::panic::catch_unwind;
  use test::Bencher;  // cf. https://github.com/rust-lang/rfcs/issues/1484

  #[test] fn str2ucs_test() {
    let mut to_ucs = Vec::new();
    let mut from_ucs = String::new();
    assert_eq! (unsafe {libc::wcslen (str2ucs (&mut to_ucs, "foo") .as_ptr())}, 3);
    assert_eq! (ucs2string (&mut from_ucs, str2ucs (&mut to_ucs, "foo") .as_ptr(), false), "foo");}

  #[test] fn rss_reader() {  // http://vtd-xml.sourceforge.net/codeSample/cs2.html, RSSReader.c
    vtd_catch (&mut || {
      let vg = unsafe {createVTDGen()};
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
      let mut to_ucs = Vec::new();
      let mut from_ucs = String::new();
      let ns1 = str2ucs (&mut to_ucs, "ns1") .clone();
      let url = str2ucs (&mut to_ucs, "http://purl.org/dc/elements/1.1/") .clone();
      unsafe {declareXPathNameSpace (ap, ns1.as_ptr(), url.as_ptr())};
      let mut num = 0;
      if unsafe {selectXPath (ap, str2ucs (&mut to_ucs, "//ns1:*") .as_ptr())} == Bool::TRUE {
        unsafe {bind (ap, vn)};
        let mut result; while {result = unsafe {evalXPath (ap)}; result} != -1 {
          let tmp_string = unsafe {toString (vn, result)};
          let name = ucs2string (&mut from_ucs, tmp_string, true) .clone();
          let t = unsafe {getText (vn)};
          let mut text = String::new();
          if t != -1 {
            let tmp_string = unsafe {toNormalizedString (vn, t)};
            text.push_str (&ucs2string (&mut from_ucs, tmp_string, true));}
          //println! ("evalXPath result: {}; name: {}; text: {}", result, name, text);
          match {num += 1; num} {
            1 => {assert_eq! (name, "dc:creator"); assert_eq! (text, "Jennifer Mears")},
            2 => {assert_eq! (name, "dc:date"); assert_eq! (text, "2004-06-14T00:00:00Z")},
            x => panic! ("num is {}", x)}}}
      assert_eq! (num, 2);
      //unsafe {freeVTDNav_shim (vn)};  // Often crashes on Windows.
      unsafe {freeVTDGen (vg)};
      unsafe {freeAutoPilot (ap)};
    }) .expect ("!rss_reader");}

  #[test] fn walk() {
    vtd_catch (&mut || {
      let xml = "<foo><bar surname=\"Stover\">Smokey</bar></foo>";
      let vg = unsafe {createVTDGen()};
      unsafe {setDoc (vg, xml.as_ptr(), xml.len() as c_int)};
      unsafe {parse (vg, Bool::FALSE)};
      let vn = unsafe {getNav (vg)};
      let mut to_ucs = Vec::new();
      let mut from_ucs = String::new();
      assert! (unsafe {toElement2_shim (vn, Direction::FirstChild, str2ucs (&mut to_ucs, "bar") .as_ptr())} == Bool::TRUE);
      assert_eq! (ucs2string (&mut from_ucs, unsafe {toString (vn, getCurrentIndex_shim (vn))}, true), "bar");
      let surname = unsafe {getAttrVal (vn, str2ucs (&mut to_ucs, "surname") .as_ptr())};
      assert! (surname != -1);
      assert_eq! (ucs2string (&mut from_ucs, unsafe {toString (vn, surname)}, true), "Stover");
      let text = unsafe {getText (vn)};
      assert! (text != -1);
      assert_eq! (ucs2string (&mut from_ucs, unsafe {toString (vn, text)}, true), "Smokey");
      //unsafe {freeVTDNav_shim (vn)};  // Often crashes on Windows.
      unsafe {freeVTDGen (vg)};
    }) .expect ("!walk");}

  #[bench] fn panic (bencher: &mut Bencher) {  // See if `vtd_catch` would propagate the Rust panics from the closure.
    std::panic::set_hook (Box::new (|_| ()));  // Prevents the panics from cussing to the stderr.
    bencher.iter (|| {
      match catch_unwind (|| {
        vtd_catch (&mut || panic! ("woot")) .expect ("!vtd_catch");}) {
          Ok (_) => panic! ("No panic!"),
          Err (panic) => {
            let message = ::any_to_str (&panic) .expect ("!message");
            assert_eq! (message, "vtd_catch] woot");}}});
    let _ = std::panic::take_hook();}  // Restore the panic handler.

  #[bench] fn unicode (bencher: &mut Bencher) {
    let xml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><foo arg=\"Рок\">Стар</foo>";
    vtd_catch (&mut || {
      let vg = unsafe {createVTDGen()};
      unsafe {setDoc (vg, xml.as_ptr(), xml.len() as c_int)};
      unsafe {parse (vg, Bool::FALSE)};
      let vn = unsafe {getNav (vg)};
      let mut to_ucs = Vec::new();
      let mut from_ucs = String::new();
      bencher.iter (|| {
        let arg = unsafe {getAttrVal (vn, str2ucs (&mut to_ucs, "arg") .as_ptr())};
        assert! (arg != -1);
        assert_eq! (ucs2string (&mut from_ucs, unsafe {toString (vn, arg)}, true), "Рок");
        let text = unsafe {getText (vn)};
        assert! (text != -1);
        assert_eq! (ucs2string (&mut from_ucs, unsafe {toString (vn, text)}, true), "Стар");});
      //unsafe {freeVTDNav_shim (vn)};  // SEGVs in `free(vn->h1);`
      unsafe {freeVTDGen (vg)};}) .expect ("!unicode");}}
