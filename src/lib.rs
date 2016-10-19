// [build] cargo test -- --nocapture

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

extern crate encoding;
extern crate libc;

use libc::c_void;
use std::mem::transmute;
use std::ptr::null;
use std::ffi::CStr;

// Bindgen example:
// cd /tmp && git clone https://github.com/ArtemGr/vtd_xml.rs.git && cd vtd_xml.rs && cargo build
// cd target/debug/build/vtd_xml-*/out/ximpleware-2.12-c/vtd
// bindgen autoPilot.h | less

/// Handmade FFI bindings to the C library (ximpleware_2.12).
pub mod sys {
  use libc::{c_int, c_char, c_void, wchar_t};
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
    pub fn parseFile (vg: *mut VTDGen, ns: Boolean, fileName: *const c_char) -> Boolean;
    pub fn selectLcDepth (vg: *mut VTDGen, d: c_int);
    /// Returns the VTDNav object after parsing, it also cleans 
    /// internal state so VTDGen can process the next file.
    pub fn getNav (vg: *mut VTDGen) -> *mut VTDNav;
    pub fn cloneNav (vn: *mut VTDNav) -> *mut VTDNav;
    pub fn freeVTDNav_shim (vn: *mut VTDNav);

    // vtdNav.h

    /// Get the depth (>=0) of the current element.
    pub fn getCurrentDepth (vn: *mut VTDNav) -> c_int;
    /// Move the cursor to the element according to the direction constants If no such element, no position change and return false.
    pub fn toElement (vn: *mut VTDNav, direction: Direction) -> Boolean;
    /// Move the cursor to the element according to the direction constants and the element name.
    /// If no such element, no position change and return false.
    /// "*" matches any element.
    pub fn toElement2(vn: *mut VTDNav, direction: Direction, en: *const UCSChar) -> Boolean;
    /// Test if the current element matches the given name.
    pub fn matchElement (vn: *mut VTDNav, en: *mut UCSChar) -> Boolean;
    pub fn toNormalizedXPathString (vn: *mut VTDNav, j: c_int) -> *mut UCSChar;
    /// Return the attribute count of the element at the cursor position.
    pub fn getAttrCount (vn: *mut VTDNav) -> c_int;
    /// Test whether current element has an attribute with the matching name.
    pub fn hasAttr (vn: *mut VTDNav, attrName: *mut UCSChar) -> Boolean;
    /// Get the token index of the attribute value given an attribute name.
    pub fn getAttrVal (vn: *mut VTDNav, attrName: *mut UCSChar) -> c_int;

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
    pub fn vtd_try_catch_shim (cb: extern fn (*mut c_void), arg: *mut c_void, ex: *mut VtdException) -> c_int;}}

pub mod helpers {
  use ::sys::UCSChar;
  use encoding::{Encoding, DecoderTrap};
  use encoding::all::UTF_16LE;
  use libc;
  use std::mem::size_of;
  use std::ptr::null;
  use std::slice::from_raw_parts;

  /// Decodes a NIL-terminated UCSChar into a UTF-8 string.
  pub fn usc2string (us: *const UCSChar) -> String {
    if us == null() {return String::new()}
    let mut len = 0;
    while unsafe {*us.offset (len)} != 0 {len += 1}
    let slice = unsafe {from_raw_parts (us as *const u8, len as usize * size_of::<UCSChar>())};
    UTF_16LE.decode (slice, DecoderTrap::Ignore) .expect ("!UCS")}

  pub fn str2uchar (s: &str) -> Vec<UCSChar> {
    let mut v = Vec::with_capacity (s.len() + 1);
    for ch in s.chars() {v.push (ch as UCSChar)}
    v.push (0);  // NIL-terminated.
    assert_eq! (s.len(), unsafe {libc::wcslen (v.as_ptr())});
    v}}

#[derive(Debug)]
pub struct VtdError {
  pub et: sys::ExceptionType,
  pub subtype: i32,
  pub msg: String,
  pub sub_msg: String}

pub fn vtd_catch (mut cb: &mut FnMut()) -> Result<(), VtdError> {
  // http://stackoverflow.com/a/38997480/257568.
  let closure_pp: *mut c_void = unsafe {transmute (&mut cb)};
  let mut ex = sys::VtdException::default();
  if unsafe {::sys::vtd_try_catch_shim (::vtd_xml_try_catch_rust_shim, closure_pp, &mut ex)} == 0 {Ok (())} else {
    Err (VtdError {
      et: ex.et,
      subtype: ex.subtype as i32,
      msg: if ex.msg == null() {String::new()} else {
        unsafe {CStr::from_ptr (ex.msg as *mut i8)} .to_string_lossy().into_owned()},
      sub_msg: if ex.sub_msg == null() {String::new()} else {
        unsafe {CStr::from_ptr (ex.sub_msg as *mut i8)} .to_string_lossy().into_owned()}})}}

/// Called from inside a C function (`vtd_try_catch_shim`) in order to execute some Rust code while catching any VTD-XML exceptions.
#[no_mangle] #[doc(hidden)]
pub extern "C" fn vtd_xml_try_catch_rust_shim (closure_pp: *mut c_void) {
  let closure: &mut &mut FnMut() = unsafe {transmute (closure_pp)};
  // TODO: catch_unwind.
  closure();}

#[cfg(test)] mod tests {
  use ::sys::*;
  use ::helpers::*;
  use ::vtd_catch;
  use libc::{self, c_void, c_int};

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
      let ns1 = str2uchar ("ns1");
      let url = str2uchar ("http://purl.org/dc/elements/1.1/");
      unsafe {declareXPathNameSpace (ap, ns1.as_ptr(), url.as_ptr())};
      if unsafe {selectXPath (ap, str2uchar ("//ns1:*") .as_ptr())} == Bool::TRUE {
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
      unsafe {freeAutoPilot (ap)};
    }) .expect ("!rss_reader");}

  #[test] fn walk() {
    //let xml = "<foo><bar surname=\"Stover\">Smokey</bar></foo>";

  }
}
